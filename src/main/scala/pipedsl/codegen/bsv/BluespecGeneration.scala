package pipedsl.codegen.bsv

import pipedsl.common.BSVSyntax._
import pipedsl.common.DAGSyntax.{PStage, PipelineEdge}
import pipedsl.common.Errors.UnexpectedCommand
import pipedsl.common.Syntax._
import pipedsl.common.Utilities.{flattenStageList, log2}

import scala.annotation.tailrec

object BluespecGeneration {

  private val lockLib = "Locks"
  private val memLib = "Memories"
  private val fifoLib = "FIFOF"

  class BluespecProgramGenerator(prog: Prog, stageInfo: Map[Id, List[PStage]]) {

    //TODO compile functions into bsv functions into their own file
    //fill in the function map by generating each function and using
    //it to refer to prior functions
    private val funcMap: Map[Id, BFuncDef] = Map()

    //for each module
    private val handleTyps: Map[Id, BSVType] = prog.moddefs.foldLeft(Map[Id, BSVType]())((mapping, mod) => {
      val stages = stageInfo(mod.name)
      val handletyp = BSizedInt(unsigned = true, log2(flattenStageList(stages).length))
      mapping + (mod.name -> handletyp)
    })

    private val modMap: Map[Id, BProgram] = prog.moddefs.foldLeft(Map[Id, BProgram]())((mapping, mod) => {
      val modtyps = mapping map { case (i, p) => (i, p.topModule.typ.get) }
      val newmod = new BluespecModuleGenerator(
        mod, stageInfo(mod.name).head, flattenStageList(stageInfo(mod.name).tail), modtyps, handleTyps
      ).getBSV
      mapping + ( mod.name -> newmod )
    })

    private val translator = new BSVTranslator(modMap map { case (i, p) => (i, p.topModule.typ.get) }, handleTyps)

    private def translateCircuit(c: Circuit, env: Map[Id, BVar]): (List[BStatement], Map[Id, BVar]) = c match {
      case CirSeq(c1, c2) =>
        val (stmts1, env1) = translateCircuit(c1, env)
        val (stmts2, env2) = translateCircuit(c2, env1)
        (stmts1 ++ stmts2, env2)
      case CirConnect(name, c) =>
        val mod = cirExprToModule(c, env)
        val modtyp = getModuleType(c, name.typ.get)
        val modvar = BVar(name.v, modtyp)
        (List(BModInst(modvar, mod)), env + (name -> modvar))
    }

    private def getModuleType(c: CirExpr, expected: Type): BSVType = c match {
      case CirNew(mod, _, _) => BluespecInterfaces.getInterface(modMap(mod))
      case _ => translator.toBSVType(expected)
    }

    private def cirExprToModule(c: CirExpr, env: Map[Id, BVar]): BModule = c match {
      case CirMem(elemTyp, addrSize) =>
        val memtyp = translator.toBSVType(TMemType(elemTyp, addrSize, Latency.Asynchronous, Latency.Asynchronous))
        BluespecInterfaces.getMemoryModule(memtyp)
      case CirRegFile(elemTyp, addrSize) =>
        val memtyp = translator.toBSVType(TMemType(elemTyp, addrSize, Latency.Combinational, Latency.Sequential))
        BluespecInterfaces.getMemoryModule(memtyp)
      case CirNew(mod, _, mods) =>
        BModule(name = BluespecInterfaces.getModuleName(modMap(mod)), args = mods.map(m => env(m)))
    }

    private def initCircuit(c: Circuit, env: Map[Id, BVar]): List[BStatement] = c match {
      case CirSeq(c1, c2) => initCircuit(c1, env) ++ initCircuit(c2, env)
      case CirConnect(name, c) => c match {
        case CirNew(_, inits, _) => List(
          BExprStmt(BMethodInvoke(env(name),
            BluespecInterfaces.requestMethodName, inits.map(i => translator.toBSVExpr(i))))
        )
        case _ => List() //TODO if/when memories can be initialized it goes here
      }
    }

    //Create a top level module that is synthesizable, takes no parameters
    //and instantiates all of the required memories and pipeline modules
    //TODO generate other debugging/testing rules
    private val topLevelModule: BModuleDef = {
      val (cirstmts, argmap) = translateCircuit(prog.circ, Map())
      val initrule = BRuleDef(name = "init", conds = List(), body = initCircuit(prog.circ, argmap))
      BModuleDef(name = "mkCircuit", typ = None, params = List(),
        body = cirstmts, rules = List(initrule), methods = List())
    }

    val topProgram: BProgram = BProgram(name = "Circuit", topModule = topLevelModule,
      imports = BImport(memLib) +: modMap.values.map(p => BImport(p.name)).toList, exports = List(),
      structs = List(), interfaces = List(), modules = List())

    def getBSVPrograms: List[BProgram] = {
      modMap.values.toList :+ topProgram
    }
  }

  class BluespecFunctionGenerator() {
  }

  /**
   * Given a list of pipeline stages that describe a pipeline module
   * and the interface to that module, generate a
   * BSV Module that represents the entire pipeline.
   *
   * This comment and top-level function are still TODO,
   * we need to refactor a bit.
   *
   * @param firstStage  - The first stage in the pipeline that accepts inputs from
   *                    a single channel (unlike the other stages).
   * @param otherStages - The full remaining list of pipeline stages.
   * @return - The BSV Module that represents this pipeline.
   */
  private class BluespecModuleGenerator(val mod: ModuleDef,
    val firstStage: PStage, val otherStages: List[PStage],
    val bsvMods: Map[Id, BInterface], val bsvHandles: Map[Id, BSVType]) {

    private val translator = new BSVTranslator(bsvMods, bsvHandles)

    private val lockType = "Lock"
    private val lockModuleName = "mkLock"
    private val threadIdName = "_threadID"

    //Helpers for disambiguating generated edge names
    private var edgeCounter: Map[(PStage, PStage), Int] = Map().withDefaultValue(0)
    private var edgeNames: Map[PipelineEdge, String] = Map()

    type EdgeInfo = Map[PipelineEdge, BVar]
    type ModInfo = Map[Id, BVar]
    type LockInfo = Map[Id, BVar]
    type StageCode = (Iterable[BStatement], Iterable[BRuleDef])

    //Normal Map that special cases when checking the first Stage as part of the key
    //Since The First stage only has 1 incoming 'edge'
    class EdgeMap(firstStage: PStage, firstStageInput: BStruct, edges: Map[PipelineEdge, BStructDef]) {

      private val realMap: Map[PipelineEdge, BStruct] = edges map { case (k, v) => (k, v.typ) }

      def apply(e: PipelineEdge): BStruct = {
        realMap.get(e) match {
          case Some(b) => b
          case None if e.to == firstStage => firstStageInput
          case _ => throw new RuntimeException("Missing edge struct type")
        }
      }

    }

    //Registers for external communication
    private val busyReg = BVar("busyReg", BluespecInterfaces.getRegType(BBool))
    private val threadIdVar = BVar(threadIdName, getThreadIdType)
    private val outputStructHandle = BVar("handle", getThreadIdType)
    private val outputStructData =  BVar("data", translator.toBSVType(mod.ret.getOrElse(TVoid())))
    private val outQueueStructName = "OutputQueueInfo"
    private val outputQueueElem = if (mod.ret.isDefined) {
      BStruct(outQueueStructName, List(outputStructHandle, outputStructData))
    } else {
      BStruct(outQueueStructName, List(outputStructHandle))
    }
    private val outputQueueStruct = BStructDef(outputQueueElem, List("Bits", "Eq"))
    private val outputQueue = BVar("outputQueue", BluespecInterfaces.getFifoType(outputQueueElem))
    //

    //Data types for passing between stages
    private val edgeStructInfo = getEdgeStructInfo(otherStages, addTId = true)
    //First stage should have exactly one input edge by definition
    private val firstStageStruct = getEdgeStructInfo(List(firstStage), addTId = true).values.head
    private val inputFields = getEdgeStructInfo(List(firstStage), addTId = false).values.head.typ.fields
    //This map returns the correct struct type based on the destination stage
    private val edgeMap = new EdgeMap(firstStage, firstStageStruct.typ, edgeStructInfo)
    val allEdges = (firstStage +: otherStages).foldLeft(Set[PipelineEdge]())((es, s) => {
      es ++ s.inEdges ++ s.outEdges
    })
    val edgeParams: EdgeInfo = allEdges.foldLeft(Map[PipelineEdge, BVar]())((m, e) => {
      m + (e -> BVar(genParamName(e), BluespecInterfaces.getFifoType(edgeMap(e))))
    })
    //Generate map from existing module parameter names to BSV variables
    private val modParams: ModInfo = mod.modules.foldLeft[ModInfo](Map())((vars, m) => {
      vars + (m.name -> BVar(m.name.v, translator.toBSVType(m.typ)))
    })
    //mapping memory ids to their associated locks
    private val lockParams: LockInfo = mod.modules.foldLeft[LockInfo](Map())((locks, m) => {
      locks + (m.name -> BVar(genLockName(m.name), getLockType))
    })
    //Generate a Submodule for each stage
    private val stgMap = (firstStage +: otherStages).foldLeft(Map[PStage, StageCode]())((m, s) => {
      m + (s -> getStageCode(s))
    })
    //The Interface this module exposes to the outside world
    private val modInterfaceDef = BluespecInterfaces.defineInterface(
      mod.name.v,
      inputFields,
      getThreadIdType,
      mod.ret match {
        case Some(t) => Some(translator.toBSVType(t))
        case None => None
      })
    //The top level module that implements the above interface
    private val topModule = getTopModule



    private def getThreadIdType: BSVType = {
      BSizedInt(unsigned = true, log2(otherStages.length + 1))
    }

    private def getLockType: BInterface = {
      BInterface(lockType, List(BVar("idsize", getThreadIdType)))
    }

    /**
     * Returns the BSV program AST that represents the translation
     * of the provided module.
     *
     * @return
     */
    def getBSV: BProgram = {
      BProgram(name = mod.name.v.capitalize,
        topModule = topModule,
        //TODO import other used modules
        imports = List(BImport(fifoLib), BImport(lockLib), BImport(memLib)),
        exports = List(BExport(modInterfaceDef.typ.name, expFields = true), BExport(topModule.name, expFields = false)),
        structs = firstStageStruct +: edgeStructInfo.values.toList :+ outputQueueStruct,
        interfaces = List(modInterfaceDef),
        modules = List())
    }

    /**
     * Converts the edge value information into BSV structs
     * that hold all of those values and puts it into a lookup structure.
     *
     * @param stgs   - The list of stages to process.
     * @param addTId - If true, then add a ThreadID field to the struct
     * @return - A map from edges to the BSV type definitions that represent their values.
     */
    private def getEdgeStructInfo(stgs: Iterable[PStage], addTId: Boolean = true): Map[PipelineEdge, BStructDef] = {
      stgs.foldLeft[Map[PipelineEdge, BStructDef]](Map())((m, s) => {
        s.inEdges.foldLeft[Map[PipelineEdge, BStructDef]](m)((ms, e) => {
          var sfields = e.values.foldLeft(List[BVar]())((l, id) => {
            l :+ BVar(id.v, translator.toBSVType(id.typ.get))
          })
          if (addTId) sfields = sfields :+ threadIdVar
          val styp = BStruct(genStructName(e), sfields)
          val structdef = BStructDef(styp, List("Bits", "Eq"))
          ms + (e -> structdef)
        })
      })
    }

    //Helper functions that cannonically generate names from pipeline structures.
    private def genEdgeName(e: PipelineEdge) = {
      if (edgeNames.contains(e)) {
        edgeNames(e)
      } else {
        val existingedges = edgeCounter(e.from, e.to)
        val endstr = if (existingedges > 0) {
          "_" + existingedges.toString
        } else {
          ""
        }
        val name = e.from.name.v + "_TO_" + e.to.name.v + endstr
        edgeNames = edgeNames.updated(e, name)
        edgeCounter = edgeCounter.updated((e.from, e.to), existingedges + 1)
        name
      }
    }

    private def genStructName(e: PipelineEdge) = {
      "E_" + genEdgeName(e)
    }

    private def genParamName(e: PipelineEdge): String = {
      "fifo_" + genEdgeName(e)
    }

    private def genParamName(s: PStage): String = {
      "s_" + s.name
    }

    private def getStagePrefix(s: PStage): String = {
      "_" + s.name.v + "_"
    }

    private def genLockName(mem: Id): String = {
      mem.v + "_lock"
    }

    /**
     * Given a pipeline stage and the necessary edge info,
     * generate a BSV module definition.
     *
     * @param stg - The pipeline stage to convert
     * @return - The generated BSV module definition
     */
    private def getStageCode(stg: PStage): StageCode = {
      //Generate set of definitions needed by rule conditions
      //(declaring variables read from unconditional inputs)
      translator.setVariablePrefix(getStagePrefix(stg))
      val sBody = getStageBody(stg)
      //Generate the set of execution rules for reading args and writing outputs
      val execRule = getStageRule(stg)
      translator.setVariablePrefix("")
      (sBody, List(execRule))
    }

    /**
     * Given a stage and the relevant edge information, generate the
     * rule which represents executing this stage, both reading its inputs
     * and writing its outputs
     *
     * @param stg      - The stage to process
     * @return A single BSV Rule
     */
    private def getStageRule(stg: PStage): BRuleDef = {
      //This is used to declare request handle variables (they must be declared in the rule body)
      val writeCmdDecls = getEffectDecls(stg.cmds)
      val writeCmdStmts = getEffectCmds(stg.cmds)
      val queueStmts = getEdgeQueueStmts(stg, stg.allEdges)
      val blockingConds = getBlockingConds(stg.cmds)
      BRuleDef( genParamName(stg) + "_execute", blockingConds, writeCmdDecls ++ writeCmdStmts ++ queueStmts)
    }

    /**
     * If any commands could cause blocking conditions that prevent
     * the rule from running, place those here (e.g. checking if locks can be acquired)
     *
     * @param cmds The list of commands to translate
     * @return The list of translated blocking commands
     */
    private def getBlockingConds(cmds: Iterable[Command]): List[BExpr] = {
      cmds.foldLeft(List[BExpr]())((l, c) => c match {
        case ICheckLock(mem) =>
          l :+ BMethodInvoke(lockParams(mem), "owns", List(translator.toBSVVar(threadIdVar)))
        case IRecv(handle, sender, _) =>
          l :+ BMethodInvoke(modParams(sender),
            BluespecInterfaces.checkHandleMethodName,
            List(translator.toBSVExpr(handle)))
        case COutput(_) => List(busyReg)
        case _ => l
      })
    }

    /**
     * Get the list of statements that represent dequeuing from
     * or enqueuing onto the input and output edges. Wrap them in
     * conditional statements if they are conditional edges.
     *
     * @param s        The stage we're compiling
     * @param es       The subset of s's edges that we're translating
     * @param args
     * @return A list of statements that represent queue operations for the relevant edges
     */
    private def getEdgeQueueStmts(s: PStage, es: Iterable[PipelineEdge],
      args: Option[Iterable[BExpr]] = None): List[BStatement] = {
      es.foldLeft(List[BStatement]())((l, e) => {
        val stmt = if (e.to == s) {
          val deq = BExprStmt(BluespecInterfaces.getFifoDeq(edgeParams(e)))
          if (e.condRecv.isDefined) {
            BIf(translator.toBSVExpr(e.condRecv.get), List(deq), List())
          } else {
            deq
          }
        } else {
          val op = if (args.isDefined) {
            getNamedStruct(edgeMap(e), args.get)
          } else {
            getCanonicalStruct(edgeMap(e), translator)
          }
          val enq = BExprStmt(BluespecInterfaces.getFifoEnq(edgeParams(e), op))
          if (e.condSend.isDefined) {
            BIf(translator.toBSVExpr(e.condSend.get), List(enq), List())
          } else {
            enq
          }
        }
        l :+ stmt
      })
    }

    /**
     *
     * @param stg        The stage to compile
     * @return
     */
    private def getStageBody(stg: PStage): List[BStatement] = {
      var body: List[BStatement] = List()
      //Define all of the variables read unconditionally
      val uncondIn = stg.inEdges.filter(e => e.condRecv.isEmpty)
      //unconditional reads are just variable declarations w/ values
      uncondIn.foreach(e => {
        //First element in read queue
        val paramExpr = BMethodInvoke(edgeParams(e), "first", List())
        e.values.foreach(v => {
          //rename variables declared in this stage
          val pvar = translator.toBSVVar(v)
          body = body :+ BDecl(pvar, Some(BStructAccess(paramExpr,
            //but don't rename the struct field names
            BVar(v.v, translator.toBSVType(v.typ.get)))))
        })
        //only read threadIDs from an unconditional edge
        body = body :+ BDecl(translator.toBSVVar(threadIdVar),
          Some(BStructAccess(paramExpr, threadIdVar)))
      })
      //generate a conditional assignment expression to choose
      //which conditional edge we're reading inputs from
      val condIn = stg.inEdges.filter(e => e.condRecv.isDefined)
      //each edge better be accepting the same values
      val variableList = condIn.foldLeft(Set[Id]())((s, e) => {
        s ++ e.values
      })
      variableList.foreach(v => {
        val condEdgeExpr = condIn.foldLeft[BExpr](BDontCare)((expr, edge) => {
          val paramExpr = BluespecInterfaces.getFifoPeek(edgeParams(edge))
          BTernaryExpr(translator.toBSVExpr(edge.condRecv.get),
            BStructAccess(paramExpr, BVar(v.v, translator.toBSVType(v.typ.get))),
            expr)
        })
        body = body :+ BDecl(translator.toBSVVar(v), Some(condEdgeExpr))
      })
      //And now add all of the combinational connections
      body ++ getCombinationalDeclarations(stg.cmds) ++ getCombinationalCommands(stg.cmds)
    }

    //TODO add in the module parameters into the definition w/ appropriate types
    //TODO define the top level module interface based on the declaration
    private def getTopModule: BModuleDef = {
      //Body instantiates all of the params (fifos & memories) and then all of the stages
      //One fifo per edge in the graph

      val edgeFifos = allEdges.foldLeft(Map[PipelineEdge, BModInst]())((m, e) => {
        m + (e -> BModInst(edgeParams(e), BluespecInterfaces.getFifo))
      })
      //TODO associate locks w/ memories
      //Instantiate a lock for each memory:
      val memLocks = lockParams.keys.foldLeft(Map[Id, BModInst]())((m, id) => {
        m + (id -> BModInst(lockParams(id), BModule(lockModuleName, List())))
      })

      //Instantiate each stage module
      val startedge = edgeFifos(firstStage.inEdges.head).lhs
      val stgStmts = stgMap.keys.foldLeft(List[BStatement]())((l, s) => {
        l ++ stgMap(s)._1
      })
      val stgrules = stgMap.keys.foldLeft(List[BRuleDef]())((l, s) => {
        l ++ stgMap(s)._2
      })
      //Instantiate the registers that describes when the module is busy/ready for inputs
      //And how it returns outputs
      val busyInst = BModInst(busyReg, BluespecInterfaces.getReg(BBoolLit(false)))
      val outputInst = BModInst(outputQueue, BluespecInterfaces.getFifo)
      val threadInst = BModInst(BVar(threadIdName, BluespecInterfaces.getRegType(getThreadIdType)),
        BluespecInterfaces.getReg(BZero))
      val stmts = (edgeFifos.values.toList ++ memLocks.values.toList
        :+ busyInst
        :+ outputInst
        :+ threadInst) ++ stgStmts
      //expose a start method as part of the top level interface
      var methods = List[BMethodDef]()
      val reqMethodDef = BMethodDef(
        sig = BluespecInterfaces.getRequestMethod(modInterfaceDef),
        cond = Some(BUOp("!", busyReg)),
        //send input data (arguments to this method) to pipeline
        body = getEdgeQueueStmts(firstStage.inEdges.head.from, firstStage.inEdges) :+
          //And set busy status to true
          //TODO, don't use this when the module doesn't have recursive calls
          // (since it's ok to pipeline separate requests in that case)
          BModAssign(busyReg, BBoolLit(true)) :+
          //increment thread id
          BModAssign(threadIdVar, BBOp("+", threadIdVar, BOne)) :+
          //and return the one being used by this request
          BReturnStmt(threadIdVar)
      )
      methods = methods :+ reqMethodDef
      val respMethodDef = BMethodDef(
        sig = BluespecInterfaces.getResponseMethod(modInterfaceDef),
        cond = None, //implicit condition of outputqueue being non empty means we don't need this
        body = List(
          BExprStmt(BluespecInterfaces.getFifoDeq(outputQueue))
        )
      )
      methods = methods :+ respMethodDef
      val peekMethodDef = if (mod.ret.isDefined) {
        Some(BMethodDef(
          sig = BluespecInterfaces.getPeekMethod(modInterfaceDef),
          cond = None, //implicit condition of outputqueue being non empty means we don't need this
          body = List(
            BReturnStmt(
              BStructAccess(BluespecInterfaces.getFifoPeek(outputQueue), outputStructData)
            ))
        ))
      } else { None }
      if (peekMethodDef.isDefined) { methods = methods :+ peekMethodDef.get}
      val handleMethodSig = BluespecInterfaces.getHandleMethod(modInterfaceDef)
      val handleMethodCheck = BMethodDef(
        sig = handleMethodSig,
        cond = None,
        body = List(
          BReturnStmt(BBOp("==",
            handleMethodSig.params.head,
            BStructAccess(BluespecInterfaces.getFifoPeek(outputQueue), outputStructHandle)
          ))
        )
      )
      methods = methods :+ handleMethodCheck
      BModuleDef(name = "mk" + mod.name.v.capitalize,
        typ = Some(modInterfaceDef.typ),
        params = modParams.values.toList,
        body = stmts,
        rules = stgrules,
        methods = methods)
    }

    /**
     * For the given commands, extract all declarations that must
     * appear in the module body and return them as BSV declaration statements.
     * @param cmds The commands to translate
     * @return The set of module body declarations these commands imply
     */
    private def getCombinationalDeclarations(cmds: Iterable[Command]): List[BDecl] = {
      var result = List[BDecl]()
      var decls = Set[BVar]()
      cmds.foreach(c => {
        val decopt = getCombinationalDeclaration(c)
        if (decopt.isDefined) {
          val dec = decopt.get
          if (!decls.contains(dec.lhs)) {
            result = result :+ dec
            decls = decls + dec.lhs
          }
        }
      })
      result
    }

    @tailrec
    private def getCombinationalDeclaration(cmd: Command): Option[BDecl] = cmd match {
      case CAssign(lhs, _) => Some(BDecl(translator.toBSVVar(lhs), None))
      case ICondCommand(_, cmd) => getCombinationalDeclaration(cmd)
      case IMemRecv(_, data) => data match {
        case Some(v) => Some(BDecl(translator.toBSVVar(v), None))
        case None => None
      }
      case IRecv(_, _, result) => Some(BDecl(translator.toBSVVar(result), None))
      case _ => None
    }

    //Helper to accumulate getCombinationalCommand results into a single list
    private def getCombinationalCommands(cmds: Iterable[Command]): List[BStatement] = {
      cmds.foldLeft(List[BStatement]())((l, c) => {
        getCombinationalCommand(c) match {
          case Some(bs) => l :+ bs
          case None => l
        }
      })
    }

    /**
     * Translate all of the commands that go into the execution
     * rules that do not write any permanent state.
     *
     * @param cmd The command to translate
     * @return Some(translation) if cmd is combinational, otherwise None
     */
    private def getCombinationalCommand(cmd: Command): Option[BStatement] = cmd match {
      case CAssign(lhs, rhs) =>
        Some(BAssign(translator.toBSVVar(lhs), translator.toBSVExpr(rhs)))
      case ICondCommand(cond: Expr, c: Command) => getCombinationalCommand(c) match {
        case Some(bc) => Some(BIf(translator.toBSVExpr(cond), List(bc), List()))
        case None => None
      }
      case CExpr(exp) => Some(BExprStmt(translator.toBSVExpr(exp)))
      case IMemRecv(mem: Id, data: Option[EVar]) => data match {
        case Some(v) => Some(BAssign(translator.toBSVVar(v), BMemPeek(modParams(mem))))
        case None => None
      }
      case IRecv(_, sender, outvar) => Some(BAssign(translator.toBSVVar(outvar),
          BMethodInvoke(modParams(sender), BluespecInterfaces.peekMethodName, List())))
      case CLockOp(_, _) => None
      case CCheck(_) => None
      case CEmpty => None
      case _: ISpeculate => None
      case _: IUpdate => None
      case _: ICheck => None
      case _: IMemSend => None
      case _: ISend => None
      case _: InternalCommand => None
      case COutput(_) => None
      case CRecv(_, _) => throw UnexpectedCommand(cmd)
      case CIf(_, _, _) => throw UnexpectedCommand(cmd)
      case CSeq(_, _) => throw UnexpectedCommand(cmd)
      case CTBar(_, _) => throw UnexpectedCommand(cmd)
      case CReturn(_) => throw UnexpectedCommand(cmd)
      case CSpeculate(_, _, _, _) => throw UnexpectedCommand(cmd)
      case CSplit(_, _) => throw UnexpectedCommand(cmd)
    }

    //Helper to accumulate geteffectdecl results into a single list
    private def getEffectDecls(cmds: Iterable[Command]): List[BStatement] = {
      cmds.foldLeft(List[BStatement]())((l, c) => {
        getEffectDecl(c) match {
          case Some(bs) => l :+ bs
          case None => l
        }
      })
    }

    /**
     * These are for declaring variables that must appear in the rule
     * body. Specifically, this applies to any value assigned as the result of an ActionValue method.
     * (e.g.  request <- mod.req(arg1, arg2);)
     * In that example, request must be declared in the rule body, not the module body.
 *
     * @param cmd - The commands to be checked
     * @return
     */
    @tailrec
    private def getEffectDecl(cmd: Command): Option[BStatement] = cmd match {
      case ICondCommand(_: Expr, c: Command) => getEffectDecl(c)
      case ISend(handle, _, _) => Some(BDecl(translator.toBSVVar(handle), None))
      case _ => None
    }

    //Helper to accumulate getWriteCmd results into a single list
    private def getEffectCmds(cmds: Iterable[Command]): List[BStatement] = {
      cmds.foldLeft(List[BStatement]())((l, c) => {
        getEffectCmd(c) match {
          case Some(bs) => l :+ bs
          case None => l
        }
      })
    }

    /**
     * Returns the translation of any commands that write to any state:
     * this includes CALLs to the pipeline, lock acquisition, or any
     * memory requests/responses
     *
     * @param cmd The command to translate
     * @return Some(translation) if cmd is effectful, else None
     */
    private def getEffectCmd(cmd: Command): Option[BStatement] = cmd match {
      case ICondCommand(cond: Expr, c: Command) => getEffectCmd(c) match {
        case Some(bc) => Some(BIf(translator.toBSVExpr(cond), List(bc), List()))
        case None => None
      }
      //TODO implement lock arguments (a.k.a. thread IDs)
      case CLockOp(mem, op) => op match {
        case pipedsl.common.Locks.LockState.Free => None
        case pipedsl.common.Locks.LockState.Reserved =>
          Some(BExprStmt(BMethodInvoke(lockParams(mem), "res", List(translator.toBSVVar(threadIdVar)))))
        case pipedsl.common.Locks.LockState.Acquired =>
          Some(BExprStmt(BMethodInvoke(lockParams(mem), "acq", List(translator.toBSVVar(threadIdVar)))))
        case pipedsl.common.Locks.LockState.Released =>
          Some(BExprStmt(BMethodInvoke(lockParams(mem), "rel", List(translator.toBSVVar(threadIdVar)))))
      }
      case IMemSend(isWrite, mem: Id, data: Option[EVar], addr: EVar) =>
        if (isWrite) {
          Some(BMemWrite(modParams(mem), translator.toBSVExpr(addr), translator.toBSVExpr(data.get)))
        } else {
          Some(BMemReadReq(modParams(mem), translator.toBSVExpr(addr)))
        }
      //This is a write op b/c is modifies the mem queue its reading from
      case IMemRecv(mem: Id, data: Option[EVar]) => data match {
        case Some(v) => Some(BMemReadResp(translator.toBSVVar(v), modParams(mem)))
        case None => None
      }
      case ISend(handle, receiver, args) =>
        //Only for sends that are recursive (i.e., not leaving this module)
        if (receiver == mod.name) {
          Some(BStmtSeq(
            getEdgeQueueStmts(firstStage.inEdges.head.from, firstStage.inEdges,
              //need to add threadid var explicitly, its not in the surface syntax
              Some(args.map(a => translator.toBSVExpr(a)) :+ translator.toBSVVar(threadIdVar)))
          ))
        } else {
          Some(BInvokeAssign(translator.toBSVVar(handle),
            BMethodInvoke(modParams(receiver), BluespecInterfaces.requestMethodName,
            args.map(a => translator.toBSVExpr(a)))))
        }
      case IRecv(_, sender, _) =>
        Some(BExprStmt(BMethodInvoke(modParams(sender), BluespecInterfaces.responseMethodName, List())))
      case COutput(exp) => {
        val outstruct = if (mod.ret.isDefined) {
          BStructLit(outputQueueElem,
            Map(outputStructData -> translator.toBSVExpr(exp),
              outputStructHandle -> translator.toBSVVar(threadIdVar))
          )
        } else {
          BStructLit(outputQueueElem, Map(outputStructHandle -> translator.toBSVVar(threadIdVar)))
        }
        Some(BStmtSeq(List(
          BModAssign(busyReg, BBoolLit(false)), //we're done processing the current request
          //place the result in the output queue
          BExprStmt(BluespecInterfaces.getFifoEnq(outputQueue, outstruct))
        )))
      }
      case _: ICheckLock => None
      case CAssign(_, _) => None
      case CExpr(_) => None
      case CEmpty => None
      case _: InternalCommand => throw UnexpectedCommand(cmd)
      case CCheck(_) => throw UnexpectedCommand(cmd)
      case CRecv(_, _) => throw UnexpectedCommand(cmd)
      case CSeq(_, _) => throw UnexpectedCommand(cmd)
      case CTBar(_, _) => throw UnexpectedCommand(cmd)
      case CIf(_, _, _) => throw UnexpectedCommand(cmd)
      case CReturn(_) => throw UnexpectedCommand(cmd)
      case CSpeculate(_, _, _, _) => throw UnexpectedCommand(cmd)
      case CSplit(_, _) => throw UnexpectedCommand(cmd)
    }

  }

}
