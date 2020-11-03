package pipedsl.codegen.bsv

import BSVSyntax._
import pipedsl.common.DAGSyntax.{PStage, PipelineEdge}
import pipedsl.common.Errors.{UnexpectedCommand, UnexpectedExpr, UnexpectedType}
import pipedsl.common.{Locks, ProgInfo}
import pipedsl.common.Syntax._
import pipedsl.common.Utilities.{flattenStageList, log2}


object BluespecGeneration {

  private val lockLib = "Locks"
  private val memLib = "Memories"
  private val fifoLib = "FIFOF"

  class BluespecProgramGenerator(prog: Prog, stageInfo: Map[Id, List[PStage]], pinfo: ProgInfo,
    debug: Boolean = false, bsInts: BluespecInterfaces, funcmodname: String = "Functions") {

    val funcModule: String = funcmodname
    private val funcImport = BImport(funcmodname)

    //for each module
    private val handleTyps: Map[Id, BSVType] = prog.moddefs.foldLeft(Map[Id, BSVType]())((mapping, mod) => {
      val stages = stageInfo(mod.name)
      val handletyp = BSizedInt(unsigned = true, log2(flattenStageList(stages).length))
      mapping + (mod.name -> handletyp)
    })

    private val modMap: Map[Id, BProgram] = prog.moddefs.foldLeft(Map[Id, BProgram]())((mapping, mod) => {
      val modtyps = mapping map { case (i, p) => (i, p.topModule.typ.get) }
      val modHandles = modtyps.foldLeft(Map[BSVType, BSVType]())((m, mtyp) => {
        m + (mtyp._2 -> handleTyps(mtyp._1))
      })
      val newmod = new BluespecModuleGenerator(
        mod, stageInfo(mod.name).head, flattenStageList(stageInfo(mod.name).tail), modtyps, modHandles,
        pinfo, bsInts, debug, funcImport
      ).getBSV
      mapping + ( mod.name -> newmod )
    })

    private val modToHandle: Map[BSVType, BSVType] = prog.moddefs.map(m => m.name)
      .foldLeft(Map[BSVType, BSVType]())((mapping, mod) => {
      val modtyp =  bsInts.getInterface(modMap(mod))
        mapping + (modtyp -> handleTyps(mod))
    })

    private val translator = new BSVTranslator(bsInts,
      modMap map { case (i, p) => (i, p.topModule.typ.get) }, modToHandle)

    private val funcMap: Map[Id, BFuncDef] = prog.fdefs.foldLeft(Map[Id, BFuncDef]())((fmap, fdef) => {
      fmap + (fdef.name -> translator.toBSVFunc(fdef))
    })


    private def instantiateModules(c: Circuit, env: Map[Id, BVar]): (List[BStatement], Map[Id, BVar]) = c match {
      case CirSeq(c1, c2) =>
        val (stmts1, env1) = instantiateModules(c1, env)
        val (stmts2, env2) = instantiateModules(c2, env1)
        (stmts1 ++ stmts2, env2)
      case CirConnect(name, c) =>
        val (modtyp, mod) = cirExprToModule(c, env)
        val modvar = BVar(name.v, modtyp)
        (List(BModInst(modvar, mod)), env + (name -> modvar))
        //These don't instantiate modules
      case CirExprStmt(_) => (List(), env)
    }

    private def cirExprToModule(c: CirExpr, env: Map[Id, BVar]): (BSVType, BModule) = c match {
      case CirMem(elemTyp, addrSize) =>
        val memtyp = bsInts.getMemType(isAsync = true, BSizedInt(unsigned = true, addrSize),
          translator.toBSVType(elemTyp), Some(bsInts.getDefaultMemHandleType))
        (memtyp, bsInts.getMem(memtyp))
      case CirRegFile(elemTyp, addrSize) =>
        val memtyp = bsInts.getMemType(isAsync = false, BSizedInt(unsigned = true, addrSize),
          translator.toBSVType(elemTyp), None)
        (memtyp, bsInts.getMem(memtyp))
      case CirNew(mod, mods) =>
        (bsInts.getInterface(modMap(mod)),
          BModule(name = bsInts.getModuleName(modMap(mod)), args = mods.map(m => env(m))))
      case CirCall(_, _) => throw UnexpectedExpr(c)
    }

    private var freshCnt = 0
    private def initCircuit(c: Circuit, env: Map[Id, BVar]): List[BStatement] = c match {
      case CirSeq(c1, c2) => initCircuit(c1, env) ++ initCircuit(c2, env)
      case CirExprStmt(CirCall(m, args)) =>
        val freshVar = BVar("_unused_" + freshCnt, modToHandle(env(m).typ))
        freshCnt += 1
        List(BDecl(freshVar, None),
          BInvokeAssign(freshVar, bsInts.getModRequest(env(m), args.map(a => translator.toBSVExpr(a)))
        ))
      case _ => List() //TODO if/when memories can be initialized it goes here
    }

    //Create a top level module that is synthesizable, takes no parameters
    //and instantiates all of the required memories and pipeline modules
    //TODO generate other debugging/testing rules
    private val topLevelModule: BModuleDef = {
      val (cirstmts, argmap) = instantiateModules(prog.circ, Map())
      val startedRegInst = BModInst(BVar("started", bsInts.getRegType(BBool)),
        bsInts.getReg(BBoolLit(false)))
      val startedReg = startedRegInst.lhs
      val initCond = BUOp("!", startedReg)
      val setStartReg = BModAssign(startedReg, BBoolLit(true))
      val debugStart = if (debug) { BDisplay("Starting Pipeline %t", List(BTime)) } else BEmpty
      val initrule = BRuleDef(name = "init", conds = List(initCond),
        body = initCircuit(prog.circ, argmap) :+ setStartReg :+ debugStart)
      BModuleDef(name = "mkCircuit", typ = None, params = List(),
        body = cirstmts :+ startedRegInst, rules = List(initrule), methods = List())
    }

    val topProgram: BProgram = BProgram(name = "Circuit", topModule = topLevelModule,
      imports = BImport(memLib) +: modMap.values.map(p => BImport(p.name)).toList :+ funcImport, exports = List(),
      structs = List(), interfaces = List(), modules = List())

    def getBSVPrograms: List[BProgram] = {
      modMap.values.toList :+ topProgram
    }

    def getBSVFunctions: List[BFuncDef] = {
      funcMap.values.toList
    }
  }

  /**
   * Given a list of pipeline stages that describe a pipeline module
   * and the interface to that module, generate a
   * BSV Module that represents the entire pipeline.
   * @param firstStage  - The first stage in the pipeline that accepts inputs from
   *                    a single channel (unlike the other stages).
   * @param otherStages - The full remaining list of pipeline stages.
   * @return - The BSV Module that represents this pipeline.
   */
  private class BluespecModuleGenerator(val mod: ModuleDef,
    val firstStage: PStage, val otherStages: List[PStage],
    val bsvMods: Map[Id, BInterface], val bsvHandles: Map[BSVType, BSVType], val progInfo: ProgInfo,
    val bsInts: BluespecInterfaces, val debug:Boolean = false, val funcImport: BImport) {

    private val modInfo = progInfo.getModInfo(mod.name)
    private val translator = new BSVTranslator(bsInts, bsvMods, bsvHandles)

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
    private val busyReg = BVar("busyReg", bsInts.getRegType(BBool))
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
    private val outputQueue = BVar("outputQueue", bsInts.getFifoType(outputQueueElem))
    //

    //Data types for passing between stages
    private val edgeStructInfo = getEdgeStructInfo(otherStages, addTId = true)
    //First stage should have exactly one input edge by definition
    private val firstStageStruct = getEdgeStructInfo(List(firstStage), addTId = true).values.head
    private val inputFields = getEdgeStructInfo(List(firstStage), addTId = false).values.head.typ.fields
    //This map returns the correct struct type based on the destination stage
    private val edgeMap = new EdgeMap(firstStage, firstStageStruct.typ, edgeStructInfo)
    val allEdges: Set[PipelineEdge] = (firstStage +: otherStages).foldLeft(Set[PipelineEdge]())((es, s) => {
      es ++ s.inEdges ++ s.outEdges
    })
    val edgeParams: EdgeInfo = allEdges.foldLeft(Map[PipelineEdge, BVar]())((m, e) => {
      m + (e -> BVar(genParamName(e), bsInts.getFifoType(edgeMap(e))))
    })
    //Generate map from existing module parameter names to BSV variables
    private val modParams: ModInfo = mod.modules.foldLeft[ModInfo](Map())((vars, m) => {
      vars + (m.name -> BVar(m.name.v, translator.toBSVType(m.typ)))
    })
    //mapping memory ids to their associated locks
    private val lockParams: LockInfo = mod.modules.foldLeft[LockInfo](Map())((locks, m) => {
      val locktype = modInfo.getLockTypes.get(m.name) match {
        case Some(Locks.Specific) =>
          val addrType = m.typ match {
            case TMemType(_, addrSize, _, _) =>  BSizedInt(unsigned = true, addrSize)
            case _ => throw UnexpectedType(m.pos, "Address Lock", "Memory Type", m.typ)
          }
          bsInts.getAddrLockType(bsInts.getDefaultLockHandleType, addrType)
        case Some(Locks.General) | None =>
          bsInts.getLockType(bsInts.getDefaultLockHandleType)
      }
      locks + (m.name -> BVar(genLockName(m.name), locktype))
    })
    private val lockRegions: LockInfo = mod.modules.foldLeft[LockInfo](Map())((locks, m) => {
      locks + (m.name -> BVar(genLockRegionName(m.name),
        bsInts.getLockRegionType))
    })

    //Generate statements and rules for each stage
    private val stgMap = (firstStage +: otherStages).foldLeft(Map[PStage, StageCode]())((m, s) => {
      m + (s -> getStageCode(s))
    })
    //The Interface this module exposes to the outside world
    private val modInterfaceDef = bsInts.defineInterface(
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

    /**
     * Returns the BSV program AST that represents the translation
     * of the provided module.
     *
     * @return
     */
    def getBSV: BProgram = {
      BProgram(name = mod.name.v.capitalize,
        topModule = topModule,
        imports = List(BImport(fifoLib), BImport(lockLib), BImport(memLib), funcImport) ++
          bsvMods.values.map(bint => BImport(bint.name)).toList,
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

    private def genLockRegionName(mem: Id): String = {
      mem.v + "_lock_region"
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
      val writeCmdDecls = getEffectDecls(stg.getCmds)
      val writeCmdStmts = getEffectCmds(stg.getCmds)
      val queueStmts = getEdgeQueueStmts(stg, stg.allEdges)
      val blockingConds = getBlockingConds(stg.getCmds)
      val debugStmt = if (debug) {
        BDisplay(mod.name.v + ":Thread %d:Executing Stage " + stg.name + " %t",
          List(translator.toBSVVar(threadIdVar), BTime))
      } else BEmpty
      BRuleDef( genParamName(stg) + "_execute", blockingConds,
        writeCmdDecls ++ writeCmdStmts ++ queueStmts :+ debugStmt)
    }

    /**
     * If any commands could cause blocking conditions that prevent
     * the rule from running, place those here (e.g. checking if locks can be acquired)
     * The list is treated as a conjunction of conditions.
     * @param cmds The list of commands to translate
     * @return The list of translated blocking commands
     */
    private def getBlockingConds(cmds: Iterable[Command]): List[BExpr] = {
      cmds.foldLeft(List[BExpr]())((l, c) => c match {
        case CLockStart(mod) =>
          l :+ bsInts.getCheckStart(lockRegions(mod))
        case ICheckLockFree(mem) =>
          l :+ bsInts.getCheckEmpty(lockParams(mem.id),
            translator.toBSVVar(mem.evar))
        case IReserveLock(_, mem) =>
          bsInts.getCanReserve(lockParams(mem.id), translator.toBSVVar(mem.evar)) match {
            case Some(m) => l :+ m
            case None => l
          }
        case ICheckLockOwned(mem, handle) =>
          l :+ bsInts.getCheckOwns(lockParams(mem.id),
            translator.toBSVExpr(handle), translator.toBSVVar(mem.evar))
        case IMemRecv(mem: Id, handle: EVar, data: Option[EVar]) if data.isDefined =>
          l :+ bsInts.getCheckMemResp(modParams(mem), translator.toBSVVar(handle))
        case IRecv(handle, sender, _) =>
          l :+ bsInts.getModCheckHandle(modParams(sender), translator.toBSVExpr(handle))
        case COutput(_) => if (mod.isRecursive) List(busyReg) else List()
        case ICondCommand(cond, cs) =>
          val condconds = getBlockingConds(cs)
          if (condconds.nonEmpty) {
            val nestedConds = condconds.tail.foldLeft(condconds.head)((exp, n) => {
              BBOp("&&", exp, n)
            })
            val newCond = BBOp("||", BUOp("!", translator.toBSVExpr(cond)), nestedConds)
            l :+ newCond
          } else {
            l
          }
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
     * @param args      An optional list of expressions to use as the arguments instead of the cannonical variable names
     * @return A list of statements that represent queue operations for the relevant edges
     */
    private def getEdgeQueueStmts(s: PStage, es: Iterable[PipelineEdge],
      args: Option[Iterable[BExpr]] = None): List[BStatement] = {
      es.foldLeft(List[BStatement]())((l, e) => {
        val stmt = if (e.to == s) {
          val deq = BExprStmt(bsInts.getFifoDeq(edgeParams(e)))
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
          val enq = BExprStmt(bsInts.getFifoEnq(edgeParams(e), op))
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
     * @param stg The stage to compile
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
          val paramExpr = bsInts.getFifoPeek(edgeParams(edge))
          BTernaryExpr(translator.toBSVExpr(edge.condRecv.get),
            BStructAccess(paramExpr, BVar(v.v, translator.toBSVType(v.typ.get))),
            expr)
        })
        body = body :+ BDecl(translator.toBSVVar(v), Some(condEdgeExpr))
      })
      //And now add all of the combinational connections
      body ++ getCombinationalDeclarations(stg.getCmds) ++ getCombinationalCommands(stg.getCmds)
    }

    private def getTopModule: BModuleDef = {
      //Body instantiates all of the params (fifos & memories) and then all of the stages
      //One fifo per edge in the graph

      val edgeFifos = allEdges.foldLeft(Map[PipelineEdge, BModInst]())((m, e) => {
        m + (e -> BModInst(edgeParams(e), bsInts.getFifo))
      })
      //Instantiate a lock for each memory:
      val memLocks = lockParams.keys.foldLeft(Map[Id, BModInst]())((m, id) => {
        m + (id -> BModInst(lockParams(id), bsInts.getLockModule(lockParams(id).typ)))
      })
      //Instantiate a lock regions for each memory:
      val memRegions = lockRegions.keys.foldLeft(Map[Id, BModInst]())((m, id) => {
        m + (id -> BModInst(lockRegions(id), bsInts.getLockRegionModule))
      })

      //Instantiate each stage module
      val stgStmts = stgMap.keys.foldLeft(List[BStatement]())((l, s) => {
        l ++ stgMap(s)._1
      })
      val stgrules = stgMap.keys.foldLeft(List[BRuleDef]())((l, s) => {
        l ++ stgMap(s)._2
      })
      //Instantiate the registers that describes when the module is busy/ready for inputs
      //And how it returns outputs
      val busyInst = BModInst(busyReg, bsInts.getReg(BBoolLit(false)))
      val outputInst = BModInst(outputQueue, bsInts.getFifo)
      val threadInst = BModInst(BVar(threadIdName, bsInts.getRegType(getThreadIdType)),
        bsInts.getReg(BZero))
      var stmts: List[BStatement] = edgeFifos.values.toList ++ memLocks.values.toList ++ memRegions.values.toList
      if (mod.isRecursive) stmts = stmts :+ busyInst
      stmts = (stmts :+ outputInst :+ threadInst) ++ stgStmts
      //expose a start method as part of the top level interface
      var methods = List[BMethodDef]()
      val reqMethodDef = BMethodDef(
        sig = bsInts.getRequestMethod(modInterfaceDef),
        cond = if (mod.isRecursive) Some(BUOp("!", busyReg)) else None,
        //send input data (arguments to this method) to pipeline
        body = getEdgeQueueStmts(firstStage.inEdges.head.from, firstStage.inEdges) :+
          //And set busy status to true if recursive
          (if (mod.isRecursive) BModAssign(busyReg, BBoolLit(true)) else BEmpty) :+
          //increment thread id
          BModAssign(threadIdVar, BBOp("+", threadIdVar, BOne)) :+
          //and return the one being used by this request
          BReturnStmt(threadIdVar)
      )
      methods = methods :+ reqMethodDef
      val respMethodDef = BMethodDef(
        sig = bsInts.getResponseMethod(modInterfaceDef),
        cond = None, //implicit condition of outputqueue being non empty means we don't need this
        body = List(
          BExprStmt(bsInts.getFifoDeq(outputQueue))
        )
      )
      methods = methods :+ respMethodDef
      val peekMethodDef = if (mod.ret.isDefined) {
        Some(BMethodDef(
          sig = bsInts.getPeekMethod(modInterfaceDef),
          cond = None, //implicit condition of outputqueue being non empty means we don't need this
          body = List(
            BReturnStmt(
              BStructAccess(bsInts.getFifoPeek(outputQueue), outputStructData)
            ))
        ))
      } else { None }
      if (peekMethodDef.isDefined) { methods = methods :+ peekMethodDef.get}
      val handleMethodSig = bsInts.getHandleMethod(modInterfaceDef)
      val handleMethodCheck = BMethodDef(
        sig = handleMethodSig,
        cond = None,
        body = List(
          BReturnStmt(BBOp("==",
            handleMethodSig.params.head,
            BStructAccess(bsInts.getFifoPeek(outputQueue), outputStructHandle)
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
      cmds.foreach {
        case ICondCommand(_, cmds) =>
          val nresult = getCombinationalDeclarations(cmds)
          nresult.foreach(stmt => {
            if (!decls.contains(stmt.lhs)) {
              result = result :+ stmt
              decls = decls + stmt.lhs
            }
          })
        case c => val decopt = getCombinationalDeclaration(c)
          if (decopt.isDefined) {
            val dec = decopt.get
            if (!decls.contains(dec.lhs)) {
              result = result :+ dec
              decls = decls + dec.lhs
            }
          }
      }
      result
    }

    private def getCombinationalDeclaration(cmd: Command): Option[BDecl] = cmd match {
      case CAssign(lhs, _) => Some(BDecl(translator.toBSVVar(lhs), None))
      case IMemRecv(_, _, data) => data match {
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
      case ICondCommand(cond: Expr, cs) =>
        val stmtlist = cs.foldLeft(List[BStatement]())((l, c) => {
          getCombinationalCommand(c) match {
            case Some(bc) => l :+ bc
            case None => l
          }
        })
        if (stmtlist.nonEmpty) Some(BIf(translator.toBSVExpr(cond), stmtlist, List())) else None
      case CExpr(exp) => Some(BExprStmt(translator.toBSVExpr(exp)))
      case IMemRecv(mem: Id, _: EVar, data: Option[EVar]) => data match {
        case Some(v) => Some(BAssign(translator.toBSVVar(v), bsInts.getMemPeek(modParams(mem))))
        case None => None
      }
      case IRecv(_, sender, outvar) => Some(
        BAssign(translator.toBSVVar(outvar), bsInts.getModPeek(modParams(sender)))
      )
      case CLockStart(_) => None
      case CLockEnd(_) => None
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
    private def getEffectDecls(cmds: Iterable[Command]): List[BDecl] = {
      var result = List[BDecl]()
      var decls = Set[BVar]()
      cmds.foreach {
        case ICondCommand(_, cmds) =>
          val nresult = getEffectDecls(cmds)
          nresult.foreach(stmt => {
            if (!decls.contains(stmt.lhs)) {
              result = result :+ stmt
              decls = decls + stmt.lhs
            }
          })
        case c => val decopt = getEffectDecl(c)
          if (decopt.isDefined) {
            val dec = decopt.get
            if (!decls.contains(dec.lhs)) {
              result = result :+ dec
              decls = decls + dec.lhs
            }
          }
      }
      result
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
    private def getEffectDecl(cmd: Command): Option[BDecl] = cmd match {
      case ISend(handle, rec, _) => if (rec != mod.name) {
        Some(BDecl(translator.toBSVVar(handle), None))
      } else { None }
      case IReserveLock(handle, _) => Some(
        BDecl(translator.toBSVVar(handle), None)
      )
      case IAssignLock(handle, _) => Some(
        BDecl(translator.toBSVVar(handle), None)
      )
      case IMemSend(handle, _, _, _, _) =>
        Some(BDecl(translator.toBSVVar(handle), None))
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
      case ICondCommand(cond: Expr, cs) =>
        val stmtlist = cs.foldLeft(List[BStatement]())((l, c) => {
          getEffectCmd(c) match {
            case Some(bc) => l :+ bc
            case None => l
          }
        })
        if (stmtlist.nonEmpty) Some(BIf(translator.toBSVExpr(cond), stmtlist, List())) else None
      case CLockOp(mem, op) => op match {
        case pipedsl.common.Locks.Free => None
        case pipedsl.common.Locks.Reserved =>
          Some(BExprStmt(BMethodInvoke(lockParams(mem.id), "res", List(translator.toBSVVar(threadIdVar)))))
        case pipedsl.common.Locks.Acquired =>
          Some(BExprStmt(BMethodInvoke(lockParams(mem.id), "res", List(translator.toBSVVar(threadIdVar)))))
        case pipedsl.common.Locks.Released =>
          Some(BExprStmt(BMethodInvoke(lockParams(mem.id), "rel", List(translator.toBSVVar(threadIdVar)))))
      }
      case IMemSend(handle, isWrite, mem: Id, data: Option[EVar], addr: EVar) => Some(
        BInvokeAssign(translator.toBSVVar(handle),
          bsInts.getMemReq(modParams(mem), isWrite, translator.toBSVExpr(addr),
            data.map(e => translator.toBSVExpr(e)))
      ))
      //This is an effectful op b/c is modifies the mem queue its reading from
      case IMemRecv(mem: Id, _: EVar, _: Option[EVar]) =>
        Some(BExprStmt(bsInts.getMemResp(modParams(mem))))
      case IMemWrite(mem, addr, data) => Some(
        BExprStmt(
          bsInts.getCombWrite(modParams(mem), translator.toBSVExpr(addr), translator.toBSVExpr(data))
        ))
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
            bsInts.getModRequest(modParams(receiver), args.map(a => translator.toBSVExpr(a)))))
        }
      case IRecv(_, sender, _) =>
        Some(BExprStmt(bsInts.getModResponse(modParams(sender))))
      case COutput(exp) =>
        val outstruct = if (mod.ret.isDefined) {
          BStructLit(outputQueueElem,
            Map(outputStructData -> translator.toBSVExpr(exp),
              outputStructHandle -> translator.toBSVVar(threadIdVar))
          )
        } else {
          BStructLit(outputQueueElem, Map(outputStructHandle -> translator.toBSVVar(threadIdVar)))
        }
        Some(BStmtSeq(List(
          //we're done processing the current request
          if (mod.isRecursive) BModAssign(busyReg, BBoolLit(false)) else BEmpty,
          //place the result in the output queue
          BExprStmt(bsInts.getFifoEnq(outputQueue, outstruct))
        )))
      case IReserveLock(handle, mem) =>
        Some(
        BInvokeAssign(translator.toBSVVar(handle),
          bsInts.getReserve(lockParams(mem.id), translator.toBSVVar(mem.evar)))
      )
      case IAssignLock(handle, src) => Some(
        BAssign(translator.toBSVVar(handle), translator.toBSVExpr(src))
      )
      case IReleaseLock(mem, handle) => Some(
        BExprStmt(bsInts.getRelease(lockParams(mem.id),
          translator.toBSVVar(handle), translator.toBSVVar(mem.evar)))
      )
      case CLockStart(mod) => Some(bsInts.getStart(lockRegions(mod)))
      case CLockEnd(mod) => Some(bsInts.getStop(lockRegions(mod)))
      case _: ICheckLockFree => None
      case _: ICheckLockOwned => None
      case _: ILockNoOp => None
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
