package pipedsl.codegen.bsv

import BSVSyntax._
import pipedsl.common.DAGSyntax.{PStage, PipelineEdge}
import pipedsl.common.Errors.{UnexpectedCommand, UnexpectedExpr}
import pipedsl.common.LockImplementation.{LockInterface, MethodInfo}
import pipedsl.common.{LockImplementation, ProgInfo}
import pipedsl.common.Syntax._
import pipedsl.common.Utilities.{flattenStageList, log2, annotateSpecTimings}

import scala.collection.immutable.ListMap


object BluespecGeneration {

  private val clientLib = "ClientServer"
  private val connLib = "Connectable"
  private val lockLib = "Locks"
  private val memLib = "Memories"
  private val specLib = "Speculation"
  private val fifoLib = "FIFOF"
  private val combLib = "RegFile"
  private val asyncLib = "BRAMCore"
  private val verilogLib = "VerilogLibs"

  class BluespecProgramGenerator(prog: Prog, stageInfo: Map[Id, List[PStage]], pinfo: ProgInfo,
                                 debug: Boolean = false, bsInts: BluespecInterfaces, funcmodname: String = "Functions",
                                 memInit:Map[String, String] = Map()) {


    val funcModule: String = funcmodname
    private val funcImport = BImport(funcmodname)

    //for each module get the type of request handles used to access it
    private val handleTyps: Map[Id, BSVType] = prog.moddefs.foldLeft(Map[Id, BSVType]())((mapping, mod) => {
      val stages = stageInfo(mod.name)
      val handletyp = BSizedInt(unsigned = true, log2(flattenStageList(stages).length))
      mapping + (mod.name -> handletyp)
    })
    //for each module map the name used to define it to the bsv program representation
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
    //a different handle map that uses the BSV types as keys (instead of the original Identifiers)
    private val modToHandle: Map[BSVType, BSVType] = prog.moddefs.map(m => m.name)
      .foldLeft(Map[BSVType, BSVType]())((mapping, mod) => {
      val modtyp =  bsInts.getInterface(modMap(mod))
        mapping + (modtyp -> handleTyps(mod))
    })

    private val translator = new BSVTranslator(bsInts,
      modMap map { case (i, p) => (i, p.topModule.typ.get) }, modToHandle)

    private val extMap: Map[Id, BSVType] = prog.exts.foldLeft(Map[Id, BSVType]())((m, e) => {
      m + (e.name -> translator.toType(e.typ.get))
    })

    private val funcMap: Map[Id, BFuncDef] = prog.fdefs.foldLeft(Map[Id, BFuncDef]())((fmap, fdef) => {
      fmap + (fdef.name -> translator.toFunc(fdef))
    })

    //utility to check if this instantiates an async memory
    private def isAsyncMem(c:CirExpr): Boolean = c match {
      case CirMem(_, _, _) => true
      case _ => false
    }
    //utility to check if this instantiates a (non-async memory) module
    private def isModule(c:CirExpr): Boolean = c match {
      case CirMem(_, _, _) => false
      case _ => true
    }

    // Given the circuit specification and module types:
    // generate the set of statements that instantiate the top level modules,
    // and add any new type bindings to the environment by instantiating the modules
    private def instantiateModules(c: Circuit, env: Map[Id, BVar]): (List[BStatement], Map[Id, BVar]) = c match {
      case CirSeq(c1, c2) =>
        val (stmts1, env1) = instantiateModules(c1, env)
        val (stmts2, env2) = instantiateModules(c2, env1)
        (stmts1 ++ stmts2, env2)
      case CirConnect(name, c) =>
        val initFile = memInit.get(name.v)
        val (modtyp, mod) = cirExprToModule(c, env, initFile)
        val modvar = BVar(name.v, modtyp)
        (List(BModInst(modvar, mod)), env + (name -> modvar))
      case _ => (List(), env)
    }
    private def instantiateMems(c: Circuit, env: Map[Id, BVar]): (List[BStatement], Map[Id, BVar]) = c match {
      case CirSeq(c1, c2) =>
        val (stmts1, env1) = instantiateMems(c1, env)
        val (stmts2, env2) = instantiateMems(c2, env1)
        (stmts1 ++ stmts2, env2)
      case CirConnect(name, cm) =>
        val (elemTyp, addrSize, numPorts) = cm match {
          case CirMem(elemTyp, addrSize, numPorts) => (elemTyp, addrSize, numPorts)
          case CirLockMem(elemTyp, addrSize, _, _, numPorts) => (elemTyp, addrSize, numPorts)
          case _ => return (List(), env)
        }
        val initFile = memInit.get(name.v)
        val bElemTyp = translator.toType(elemTyp)
        val memtyp = bsInts.getMemPort( translator.getTypeSize(bElemTyp), BSizedInt(unsigned = true, addrSize), bElemTyp, numPorts)
        val memMod = bsInts.getMem(memtyp, initFile)
        val modvar = BVar(name.v, memtyp)
        (List(BModInst(modvar, memMod)), env + (name -> modvar))
      case _ => (List(), env)
    }
    //returns a map from variable names to bsv vars that represent all modules which
    // _need_ to be exposed at the top level (this correlates to those that are Called w/ some initial value
    private def getTopLevelModules(c: Circuit, memMap: Map[Id, BVar], env: Map[Id, BVar]): Map[Id, BVar] = c match {
      case CirSeq(c1, c2) =>
        val t1 = getTopLevelModules(c1, memMap, env)
        val t2 = getTopLevelModules(c2, memMap, env)
        t1 ++ t2
      case CirExprStmt(CirCall(m, _)) => Map(m -> env(m))
      case CirConnect(name, c) => c match {
        case CirLockMem(elemTyp,addrSize,_,_,numPorts) if memMap.contains(name) => //TODO less copying
          val mtyp = TMemType(elemTyp, addrSize, Latency.Asynchronous,
            Latency.Asynchronous, numPorts, numPorts)
          if(is_dp(name.typ.get)) {
            Map(
              name.copy(name.v + "1") -> BVar(name.v + "." + bsInts.getBramClientName + "1", translator.toClientType(mtyp)),
              name.copy(name.v + "2") -> BVar(name.v + "." + bsInts.getBramClientName + "2", translator.toClientType(mtyp))
            )
          }
          else
            Map(name -> BVar(name.v + "." + bsInts.getBramClientName, translator.toClientType(mtyp)))
        case CirMem(_, _, _)  if memMap.contains(name) =>
          if(is_dp(name.typ.get)) {
            Map(
              name.copy(name.v + "1") -> BVar(name.v + "." + bsInts.getBramClientName + "1", translator.toClientType(name.typ.get)),
              name.copy(name.v + "2") -> BVar(name.v + "." + bsInts.getBramClientName + "2", translator.toClientType(name.typ.get))
            )
          }
        case _ => Map()
      }
      case _ => Map()
    }

    private def getLockModArgs(mtyp: TMemType, limpl: LockInterface, szParams: List[Int]): List[BExpr] = {
      limpl.getModInstArgs(mtyp, szParams).map(a => BUnsizedInt(a))
    }

    private def getLockedMemModule(mtyp: TMemType, limpl: LockInterface,
      idSz: List[Int], initFile: Option[String]): BModule = {
      val modInstName = limpl.getModuleInstName(mtyp)
      //full args are: Lock impl args ++ Memory Init args
      val modArgs: List[BExpr] = getLockModArgs(mtyp, limpl, idSz) ++
        List(BBoolLit(initFile.isDefined), BStringLit(initFile.getOrElse("")))
      BModule(modInstName, modArgs)
    }

    private def cirExprToModule(c: CirExpr, env: Map[Id, BVar], initFile: Option[String]): (BSVType, BModule) = c match {
      case CirMem(elemTyp, addrSize, numPorts) =>
        val bElemTyp = translator.toType(elemTyp)
        val memtyp = bsInts.getBaseMemType(isAsync = true,
          translator.getTypeSize(bElemTyp), BSizedInt(unsigned = true, addrSize), bElemTyp, numPorts)
        (memtyp, bsInts.getMem(memtyp, initFile))
      case CirLockMem(elemTyp, addrSize, impl, szParams, numPorts) =>
        val lockMemTyp = translator.toType(c.typ.get)
        val mtyp = TMemType(elemTyp, addrSize, Latency.Asynchronous,
          Latency.Asynchronous, numPorts, numPorts)
        val modInstName = impl.getModuleInstName(mtyp)
        val largs = getLockModArgs(mtyp, impl, szParams)
        (lockMemTyp, BModule(modInstName, largs))
      case CirRegFile(elemTyp, addrSize) =>
        val bElemTyp = translator.toType(elemTyp)
        val memtyp = bsInts.getBaseMemType(isAsync = false,
          translator.getTypeSize(bElemTyp), BSizedInt(unsigned = true, addrSize), bElemTyp, 0)
        (memtyp, bsInts.getMem(memtyp, initFile))
      case CirLockRegFile(elemTyp, addrSize, impl, szParams) =>
        val lockMemTyp = translator.toType(c.typ.get)
        val mtyp = TMemType(elemTyp, addrSize, Latency.Combinational,
          Latency.Sequential, 5, 1)
        (lockMemTyp, getLockedMemModule(mtyp, impl, szParams, initFile))
      case CirLock(mem, impl, idsz) =>
        val lockedMemType = translator.toType(c.typ.get)
        val modInstName = impl.getModuleInstName(
          mem.typ.get.matchOrError(mem.pos, "locked mem", "mem typ") { case c:TMemType => c})
        //pass the memory itself in addition to the args the lock asks for, if the memory is in the environment
        val largs = getLockModArgs(
          mem.typ.get.matchOrError(mem.pos, "LockInstantiation", "MemTyp") { case m:TMemType => m },
          impl, idsz)
        val modargs = if (env.contains(mem)) { largs :+ env(mem) } else { largs }
        (lockedMemType, BModule(modInstName, modargs))
      case CirNew(mod, mods, params) =>
        //TODO better for externs
        val interface = if (modMap.contains(mod)) bsInts.getInterface(modMap(mod)) else extMap(mod)
        val modName =   if (modMap.contains(mod)) bsInts.getModuleName(modMap(mod)) else "mk" + mod.v
        val szParams = params.map(p => BUnsizedInt(p.v))
        (interface, BModule(name = modName, args = mods.map(m => env(m)) ++ szParams))
      case CirCall(_, _) => throw UnexpectedExpr(c)
    }

    private var freshCnt = 0
    private def varToReg(b: BVar): BVar = {
      BVar("reg" + b.name, bsInts.getRegType(b.typ))
    }

    //TODO put in a good comment here that describes the return values

    /**
     * Given a circuit, generate the statements that are used in the TestBench
     * code to execute Call Statements and start execution
     * @param c The circuit
     * @param env The mapping from program identifiers to the appropriate BSV variables made available by the top level
     *            circiut
     * @return (Rule statements, Blocking expressions, Initialization statements)
     */
    private def initCircuit(c: Circuit, env: Map[Id, BVar]): (List[BStatement], List[BExpr], List[BStatement]) = c match {
      case CirSeq(c1, c2) =>
        val left = initCircuit(c1, env)
        val right = initCircuit(c2, env)
        (left._1 ++ right._1, left._2 ++ right._2, left._3 ++ right._3)
      case CirExprStmt(CirCall(m, args)) =>
        val freshVar = BVar("_unused_" + freshCnt, modToHandle(env(m).typ))
        val varReg = varToReg(freshVar)
        freshCnt += 1
        (
          List(
            BDecl(freshVar, None),
            BInvokeAssign(freshVar, bsInts.getModRequest(env(m), args.map(a => translator.toExpr(a)))),
            BModAssign(varReg, freshVar)
          ),
          List(bsInts.getModCheckHandle(env(m), varReg)),
          List(BModInst(varReg, bsInts.getReg(BZero)))
        )
      case _ => (List(), List(), List())
    }

    private val isDualPorted :Type => Boolean = {
      case TMemType(_, _, _, _, rp, wp) => Math.max(rp, wp) > 1
      case TLockedMemType(TMemType(_, _, _, _, rp, wp), _, _) => Math.max(rp, wp) > 1
      case _ => false
    }

    private def makeConnections(c: Circuit, memMap: Map[Id, BVar], intMap: Map[Id, BVar]): List[BStatement] = {
      c match {
        case CirSeq(c1, c2) =>
          makeConnections(c1, memMap, intMap) ++ makeConnections(c2, memMap, intMap)
        case CirConnect(mem, rhs) if memMap.contains(mem) => rhs match {
          case CirMem(_, _, _) | CirLockMem(_, _, _, _, _) =>
            val leftArg = intMap.get(mem)
            val rightArg = memMap(mem)
            leftArg match {
              case Some(value) =>
                List(bsInts.makeConnection(value, rightArg, 0))
              case None =>
                val left1 = intMap(mem.copy(mem.v + "1"))
                val left2 = intMap(mem.copy(mem.v + "2"))
                List(
                  bsInts.makeConnection(left1, rightArg, 1),
                  bsInts.makeConnection(left2, rightArg, 2))
            }
          case _ => List()
        }
      case _ => List()
    }}

    //Get the body of the top level circuit and the list of modules it instantiates
    private val (mstmts, memMap) = instantiateMems(prog.circ, Map())
    private val (cirstmts, argmap) = instantiateModules(prog.circ, Map())
    private val finalArgMap = getTopLevelModules(prog.circ, memMap, argmap)
    private val topInterface: BInterfaceDef = bsInts.topModInterface(finalArgMap.values)

    //Create a top level module that is synthesizable, takes no parameters
    //and instantiates all of the required memories and pipeline modules
    private val topLevelModule: BModuleDef = {


      //Assigns the internal modules to their external facing interfaces, exposing only clients as necessary
      val assignInts = finalArgMap.values.foldLeft(List[BStatement]())((l, a) => a.typ match {
        case _ => l :+ BIntAssign(bsInts.toIntVar(a), a)
      })

      BModuleDef(name = "mkCircuit", typ = Some(bsInts.topModTyp), params = List(),
        body = cirstmts ++ assignInts, rules = List(), methods = List())
    }

    private val modarg = "_topMod"
    private val intargs = finalArgMap map { case (k, v) =>(k, BVar(modarg + "." + bsInts.toIntVar(v).name, v.typ)) }
    private val circuitstart = initCircuit(prog.circ, intargs)

    private val topProgram: BProgram = BProgram(name = "Circuit", topModule = topLevelModule,
      imports = List(BImport(clientLib), BImport(connLib), BImport(lockLib), BImport(memLib), BImport(verilogLib), BImport(combLib), BImport(asyncLib)) ++
        modMap.values.map(p => BImport(p.name)).toList :+ funcImport, exports = List(),
      structs = List(), interfaces = List(topInterface),
      modules = List(bsInts.tbModule(
        modName = modarg,
        testMod = BModule(topLevelModule.name),
        initStmts = circuitstart._1,
        modDone = circuitstart._2,
        modInsts = mstmts ++ circuitstart._3 ++ makeConnections(prog.circ, memMap, intargs),
        bsInts = bsInts,
        debug = debug
      )))

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
   * @param bsvMods - Mapping from identifiers to BSV Interfaces representing those modules (for already defined mods)
   * @param bsvHandles - Mapping from BSV Module types to their type parameter for request handles
   * @return - The BSV Module that represents this pipeline.
   */
  private class BluespecModuleGenerator(val mod: ModuleDef,
    val firstStage: PStage, val otherStages: List[PStage],
    val bsvMods: Map[Id, BInterface], val bsvHandles: Map[BSVType, BSVType], val progInfo: ProgInfo,
    val bsInts: BluespecInterfaces, val debug:Boolean = false, val funcImport: BImport) {

    private val lockHandleVars = mod.modules.foldLeft(Map[Id, BSVType]())((mapping, mod) => {
      mod.typ match {
        case TLockedMemType(_, idSz, _) => if (idSz.isEmpty) {
          //instantiate type variable
          val name = "_lidTyp_" + mod.name.v
          mapping + (mod.name -> BTypeParam(name, List(PBits("_sz" + name))))
        } else {
          //don't
          mapping
        }
        case _ => mapping
      }
    })

    private val translator = new BSVTranslator(bsInts, bsvMods ++ lockHandleVars, bsvHandles)
    private var tmpCount = 0
    private def freshTmp(t: BSVType): BVar = {
      val tmp = BVar("__tmp_" + tmpCount.toString, t)
      tmpCount = tmpCount + 1
      tmp
    }
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

    //TODO make the sid size parameterizable
    //Specuation information
    private val specAnnotations = annotateSpecTimings((firstStage +: otherStages).filter(s => s.succs.isEmpty))
    private val maxAnnotation = specAnnotations.values.flatten.foldLeft(1)((i, v) => { if ((v + 1) > i) v + 1 else i })    
    private val specIdName = "_specId"
    private val specIdTyp: BSVType = bsInts.getDefaultSpecHandleType
    private val specTable: BVar = BVar("_specTable",  bsInts.getSpecTableType(specIdTyp, maxAnnotation))
    private val specIdVar = BVar(specIdName, BMaybe(specIdTyp))
    private def getSpecIdVal = BFromMaybe(BDontCare, translator.toBSVVar(specIdVar))
    //Registers for external communication
    private val busyReg = BVar("busyReg", bsInts.getRegType(BBool))
    private val threadIdVar = BVar(threadIdName, getThreadIdType)
    private val outputStructHandle = BVar("handle", getThreadIdType)
    private val outputStructData =  BVar("data", translator.toType(mod.ret.getOrElse(TVoid())))
    private val outQueueStructName = "OutputQueueInfo"
    private val outputQueueElem = if (mod.ret.isDefined) {
      BStruct(outQueueStructName, List(outputStructHandle, outputStructData))
    } else {
      BStruct(outQueueStructName, List(outputStructHandle))
    }
    private val outputQueueStruct = BStructDef(outputQueueElem, List("Bits", "Eq"))
    private val outputQueue = BVar("outputQueue", bsInts.getFifoType(outputQueueElem))

    //Data types for passing between stages
    private val edgeStructInfo = getEdgeStructInfo(otherStages, addTId = true, addSpecId = mod.maybeSpec)
    //First stage should have exactly one input edge by definition
    private val firstStageStruct = getFirstEdgeStructInfo(firstStage, mod.inputs.map(i => i.name), addSpecId = mod.maybeSpec)
    //remove both threadid (and spec Id if necessary)
    private val inputFields = if (mod.maybeSpec) {
      firstStageStruct.typ.fields.init.init
    } else {
      firstStageStruct.typ.fields.init
    }
    //This map returns the correct struct type based on the destination stage
    private val edgeMap = new EdgeMap(firstStage, firstStageStruct.typ, edgeStructInfo)
    val allEdges: Set[PipelineEdge] = (firstStage +: otherStages).foldLeft(Set[PipelineEdge]())((es, s) => {
      es ++ s.inEdges ++ s.outEdges
    })
    val edgeParams: EdgeInfo = allEdges.foldLeft(Map[PipelineEdge, BVar]())((m, e) => {
      m + (e -> BVar(genParamName(e), bsInts.getFifoType(edgeMap(e))))
    })
    //Generate map from existing module parameter names to BSV variables
    private val modParams: ModInfo = mod.modules.foldLeft[ModInfo](ListMap())((vars, m) => {
      //use listmap to preserve order
      vars + (m.name -> BVar(m.name.v, translator.toTypeForMod(m.typ, m.name)))
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
        case Some(t) => Some(translator.toType(t))
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
        imports = List(BImport(fifoLib), BImport(lockLib), BImport(memLib),
          BImport(verilogLib), BImport(specLib), BImport(combLib), funcImport) ++
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
    private def getEdgeStructInfo(stgs: Iterable[PStage],
      addTId: Boolean = true, addSpecId: Boolean = true): Map[PipelineEdge, BStructDef] = {
      stgs.foldLeft[Map[PipelineEdge, BStructDef]](Map())((m, s) => {
        s.inEdges.foldLeft[Map[PipelineEdge, BStructDef]](m)((ms, e) => {
          var sfields = e.values.foldLeft(List[BVar]())((l, id) => {
            l :+ BVar(id.v, translator.toType(id.typ.get))
          })
          if (addTId) sfields = sfields :+ threadIdVar
          if (addSpecId) sfields = sfields :+ specIdVar
          val styp = BStruct(genStructName(e), sfields)
          val structdef = BStructDef(styp, List("Bits", "Eq"))
          ms + (e -> structdef)
        })
      })
    }

    private def getFirstEdgeStructInfo(stg: PStage, fieldOrder: Iterable[Id], addSpecId: Boolean = true): BStructDef = {
      val inedge = stg.inEdges.head //must be only one for the first stage
      val fieldstmp = fieldOrder.foldLeft(List[BVar]())((l, id) => {
        l :+ BVar(id.v, translator.toType(id.typ.get))
      }) :+ threadIdVar
      val sfields = if (addSpecId) fieldstmp :+ specIdVar else fieldstmp
      val styp = BStruct(genStructName(inedge), sfields)
      BStructDef(styp, List("Bits", "Eq"))
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

    private def genLockRegionName(mem: Id): String = {
      mem.v + "_lock_region"
    }

    private var stgSpecOrder: Int = 0
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
      val specAnnotation = specAnnotations(stg)
      if (specAnnotation.isDefined) {
        stgSpecOrder = specAnnotation.get
      }
      val sBody = getStageBody(stg)
      //Generate the set of execution rules for reading args and writing outputs
      val execRule = getStageRule(stg)
      //Add a stage kill rule if it needs one
      val killRule = getStageKillRule(stg)
      val rules = if (mod.maybeSpec && killRule.isDefined) List(execRule, killRule.get) else List(execRule)
      stgSpecOrder = 0
      translator.setVariablePrefix("")
      (sBody, rules)
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
      val recvConds = getRecvConds(stg.getCmds)
      val debugStmt = if (debug) {
        BDisplay(Some(mod.name.v + ":Thread %d:Executing Stage " + stg.name + " %t"),
          List(translator.toBSVVar(threadIdVar), BTime))
      } else BEmpty
      BRuleDef( genParamName(stg) + "_execute", blockingConds ++ recvConds,
        writeCmdDecls ++ writeCmdStmts ++ queueStmts :+ debugStmt)
    }

    /**
     * If a stage can be killed instead of executed we generate
     * a separate rule for that behavior instead of executing it conditionally
     * inside the original rule.
     * @param stg - The stage to process
     * @return Some(BSV) rule if the stage can be killed, else None
     */
    private def getStageKillRule(stg: PStage): Option[BRuleDef] = {
      val killConds = getKillConds(stg.getCmds)
      if (killConds.isEmpty) {
        None
      } else {
        val recvConds = getRecvConds(stg.getCmds)
        val debugStmt = if (debug) {
          BDisplay(Some(mod.name.v + ":SpecId %d: Killing Stage " + stg.name + "%t"),
            List(getSpecIdVal, BTime))
        } else { BEmpty }
        val deqStmts = getEdgeQueueStmts(stg, stg.inEdges) ++ getRecvCmds(stg.getCmds)
        val freeStmt = BExprStmt(bsInts.getSpecFree(specTable, getSpecIdVal))
        Some(BRuleDef( genParamName(stg) + "_kill", killConds ++ recvConds, deqStmts :+ freeStmt :+ debugStmt))
      }
    }

    private def getKillConds(cmds: Iterable[Command]): List[BExpr] = {
      cmds.foldLeft(List[BExpr]())((l, c) => c match {
          //check definitely misspeculated
          // isValid(spec) && !fromMaybe(True, check(spec))
        case CCheckSpec(_) =>
          l :+ BBOp("&&", BIsValid(translator.toBSVVar(specIdVar)),
            //order is LATE if stage has no update
            BUOp("!", BFromMaybe(BBoolLit(true),
              bsInts.getSpecCheck(specTable, getSpecIdVal, stgSpecOrder))))
          //also need these in case we're waiting on responses we need to dequeue
        case ICondCommand(cond, cs) =>
          val condconds = getKillConds(cs)
          if (condconds.nonEmpty) {
            val nestedConds = condconds.tail.foldLeft(condconds.head)((exp, n) => {
              BBOp("&&", exp, n)
            })
            val newCond = BBOp("||", BUOp("!", translator.toExpr(cond)), nestedConds)
            l :+ newCond
          } else {
            l
          }
        case _ => l
      })
    }

    private def translateMethod(mod: BVar, mi: MethodInfo): BMethodInvoke = {
      BMethodInvoke(mod, mi.name, mi.usesArgs.map(a => translator.toExpr(a)))
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
        case cl@ICheckLockFree(mem) =>
          val methodInfo = LockImplementation.getLockImpl(mem).getCheckEmptyInfo(cl)
          if (methodInfo.isDefined) {
            l :+ translateMethod(modParams(mem.id), methodInfo.get)
          } else {
            l
          }
        case cl@IReserveLock(_, mem) =>
          val methodInfo = LockImplementation.getLockImpl(mem).getCanReserveInfo(cl)
          if (methodInfo.isDefined) {
            l :+ translateMethod(modParams(mem.id), methodInfo.get)
          } else {
            l
          }
        case cl@ICheckLockOwned(mem, _) =>
          val methodInfo = LockImplementation.getLockImpl(mem).getCheckOwnsInfo(cl)
          if (methodInfo.isDefined) {
            l :+ translateMethod(modParams(mem.id), methodInfo.get)
          } else {
            l
          }
        case COutput(_) => if (mod.isRecursive) l :+ busyReg else l
          //Execute ONLY if check(specid) == Valid(True) && isValid(specid)
          // fromMaybe(False, check(specId)) <=>  check(specid) == Valid(True)
        case CCheckSpec(isBlocking) if isBlocking => l ++ List(
          BBOp("||", BUOp("!", BIsValid(translator.toBSVVar(specIdVar))),
            BFromMaybe(BBoolLit(false), bsInts.getSpecCheck(specTable, getSpecIdVal, stgSpecOrder))
          )
        )
          //Execute if check(specid) != Valid(False)
          //fromMaybe(True, check(specId)) <=> check(specid) == (Valid(True) || Invalid)
        case CCheckSpec(isBlocking) if !isBlocking => l ++ List(
          BBOp("||", BUOp("!", BIsValid(translator.toBSVVar(specIdVar))),
            //order is LATE if stage has no update
              BFromMaybe(BBoolLit(true), bsInts.getSpecCheck(specTable, getSpecIdVal, stgSpecOrder))
          )
        )
        case ICondCommand(cond, cs) =>
          val condconds = getBlockingConds(cs)
          if (condconds.nonEmpty) {
            val nestedConds = condconds.tail.foldLeft(condconds.head)((exp, n) => {
              BBOp("&&", exp, n)
            })
            val newCond = BBOp("||", BUOp("!", translator.toExpr(cond)), nestedConds)
            l :+ newCond
          } else {
            l
          }
        case _ => l
      })
    }

    private def getRecvConds(cmds: Iterable[Command]): List[BExpr] = {
      cmds.foldLeft(List[BExpr]())((l, c) => c match {
        case IMemRecv(mem: Id, handle: EVar, _: Option[EVar]) =>
          l :+ bsInts.getCheckMemResp(modParams(mem), translator.toVar(handle), c.portNum.get, translator.isLockedMem(mem))
        case IRecv(handle, sender, _) =>
          l :+ bsInts.getModCheckHandle(modParams(sender), translator.toExpr(handle))
        case ICondCommand(cond, cs) =>
          val condconds = getRecvConds(cs)
          if (condconds.nonEmpty) {
            val nestedConds = condconds.tail.foldLeft(condconds.head)((exp, n) => {
              BBOp("&&", exp, n)
            })
            val newCond = BBOp("||", BUOp("!", translator.toExpr(cond)), nestedConds)
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
            BIf(translator.toExpr(e.condRecv.get), List(deq), List())
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
            BIf(translator.toExpr(e.condSend.get), List(enq), List())
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
      var declaredVars: Set[BVar] = Set() //to avoid duplicate declarations maintain a set
      //these come up b/c variables may be sent both conditionally and unconditionally
      //(this happens when one branch has a combinational computation but the other receives a value from elsewhere)
      //Define all of the variables read unconditionally
      val uncondIn = stg.inEdges.filter(e => e.condRecv.isEmpty)
      //unconditional reads are just variable declarations w/ values
      uncondIn.foreach(e => {
        //First element in read queue
        val paramExpr = BMethodInvoke(edgeParams(e), "first", List())
        e.values.foreach(v => {
          //rename variables declared in this stage
          val pvar = translator.toVar(v)
            body = body :+ BDecl(pvar, Some(BStructAccess(paramExpr,
              //but don't rename the struct field names
              BVar(v.v, translator.toType(v.typ.get)))))
          declaredVars = declaredVars + pvar
        })
        //only read threadIDs and specIDs from an unconditional edge
        body = body :+ BDecl(translator.toBSVVar(threadIdVar),
          Some(BStructAccess(paramExpr, threadIdVar)))
        if (mod.maybeSpec) {
          body = body :+ BDecl(translator.toBSVVar(specIdVar),
            Some(BStructAccess(paramExpr, specIdVar)))
        }
      })
      //generate a conditional assignment expression to choose
      //which conditional edge we're reading inputs from
      val condIn = stg.inEdges.filter(e => e.condRecv.isDefined)
      //the list of all values sent by _some_ edge
      val variableList = condIn.foldLeft(Set[Id]())((s, e) => {
        s ++ e.values
      })
      variableList.foreach(v => {
        val condEdgeExpr = condIn.foldLeft[BExpr](BDontCare)((expr, edge) => {
          //if value is expected to come in on that edge, use that
          //else we're not going to use the value later, send a dont care
          val edgeValue = if (edge.values.contains(v)) {
            val paramExpr = bsInts.getFifoPeek(edgeParams(edge))
            BStructAccess(paramExpr, BVar(v.v, translator.toType(v.typ.get)))
          } else {
            BDontCare
          }
          BTernaryExpr(translator.toExpr(edge.condRecv.get), edgeValue, expr)
        })
        val bvar = translator.toVar(v)
        body = body :+ BDecl(bvar, Some(condEdgeExpr))
        declaredVars = declaredVars + bvar
      })
      //only add declarations for recv statements that don't confliect w/ those
      //received on the input edges.
      val recvdecls = getCombinationalDeclarations(stg.getCmds).filter(dec => !declaredVars.contains(dec.lhs))
      //And now add all of the combinational connections
      body ++ recvdecls ++ getCombinationalCommands(stg.getCmds)
    }

    private def getTopModule: BModuleDef = {
      //Body instantiates all of the params (fifos & memories) and then all of the stages
      //One fifo per edge in the graph

      //start fifo uses our 'nonblocking' queue impl
      val startEdge = firstStage.inEdges.head
      val startFifo = BModInst(edgeParams(startEdge), bsInts.getNBFifo)
      val edgeFifos = allEdges.foldLeft(Map[PipelineEdge, BModInst]())((m, e) => {
        if (e != startEdge) {
          m + (e -> BModInst(edgeParams(e), bsInts.getFifo))
        } else {
          m
        }
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
      //Instantiate the speculation table if the module is speculative
      val specInst = if (mod.maybeSpec) BModInst(specTable, bsInts.getSpecTable) else BEmpty
      var stmts: List[BStatement] = startFifo +: (edgeFifos.values.toList ++ memRegions.values.toList)
      if (mod.isRecursive) stmts = stmts :+ busyInst
      if (mod.maybeSpec) stmts = stmts :+ specInst
      stmts = (stmts :+ outputInst :+ threadInst) ++ stgStmts
      //expose a start method as part of the top level interface
      var methods = List[BMethodDef]()
      val inargs: List[BExpr] = inputFields :+ threadIdVar :+ BInvalid
      val reqMethodDef = BMethodDef(
        sig = bsInts.getRequestMethod(modInterfaceDef),
        cond = if (mod.isRecursive) Some(BUOp("!", busyReg)) else None,
        //send input data (arguments to this method) to pipeline
        body = getEdgeQueueStmts(firstStage.inEdges.head.from, firstStage.inEdges, Some(inargs)) :+
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
      case CAssign(lhs, _) => Some(BDecl(translator.toVar(lhs), None))
      case IMemRecv(_, _, data) => data match {
        case Some(v) => Some(BDecl(translator.toVar(v), None))
        case None => None
      }
      case IRecv(_, _, result) => Some(BDecl(translator.toVar(result), None))
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
        Some(BAssign(translator.toVar(lhs), translator.toExpr(rhs)))
      case ICondCommand(cond: Expr, cs) =>
        val stmtlist = cs.foldLeft(List[BStatement]())((l, c) => {
          getCombinationalCommand(c) match {
            case Some(bc) => l :+ bc
            case None => l
          }
        })
        if (stmtlist.nonEmpty) Some(BIf(translator.toExpr(cond), stmtlist, List())) else None
      case CExpr(exp) => Some(BExprStmt(translator.toExpr(exp)))
      case IMemRecv(mem: Id, handle: EVar, data: Option[EVar]) => data match {
        case Some(v) => Some(BAssign(translator.toVar(v),
          bsInts.getMemPeek(modParams(mem), translator.toVar(handle), cmd.portNum.get, translator.isLockedMem(mem))
        ))
        case None => None
      }
      case IRecv(_, sender, outvar) => Some(
        BAssign(translator.toVar(outvar), bsInts.getModPeek(modParams(sender)))
      )
      //if preds != args, update preds definition
      case CUpdate(_, _, _, _) => None
      case CLockStart(_) => None
      case CLockEnd(_) => None
      case CLockOp(_, _, _) => None
      case CSpecCall(_, _, _) => None
      case CVerify(_, _, _, _) => None
      case CInvalidate(_) => None
      case CCheckSpec(_) => None
      case CEmpty() => None
      case _: IUpdate => None
      case _: ICheck => None
      case _: IMemSend => None
      case _: ISend => None
      case _: InternalCommand => None
      case COutput(_) => None
      case CPrint(_) => None
      case CRecv(_, _) => throw UnexpectedCommand(cmd)
      case CIf(_, _, _) => throw UnexpectedCommand(cmd)
      case CSeq(_, _) => throw UnexpectedCommand(cmd)
      case CTBar(_, _) => throw UnexpectedCommand(cmd)
      case CReturn(_) => throw UnexpectedCommand(cmd)
      case CSplit(_, _) => throw UnexpectedCommand(cmd)
      case CCheckpoint(_,_) => throw UnexpectedCommand(cmd)
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
        Some(BDecl(translator.toVar(handle), None))
      } else { None }
      case IReserveLock(handle, _) => Some(
        BDecl(translator.toVar(handle), Some(BInvalid))
      )
      case IAssignLock(handle, _, default) => Some(
        BDecl(translator.toVar(handle), default match {
          case Some(value) =>Some(translator.toExpr(value))
          case None =>None
        }))
      case IMemSend(handle, _, _, _, _) =>
        Some(BDecl(translator.toVar(handle), None))
      case CSpecCall(handle, _, _) =>
        Some(BDecl(translator.toVar(handle), None))
      case CUpdate(newHandle, _, _, _) =>
        Some(BDecl(translator.toVar(newHandle), None))
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
        if (stmtlist.nonEmpty) Some(BIf(translator.toExpr(cond), stmtlist, List())) else None
      case IMemSend(handle, wMask, mem: Id, data: Option[EVar], addr: EVar) => Some(
        BInvokeAssign(translator.toVar(handle),
          bsInts.getMemReq(modParams(mem), translator.toExpr(wMask), translator.toExpr(addr),
            data.map(e => translator.toExpr(e)), cmd.portNum.get, translator.isLockedMem(mem))
      ))
      //This is an effectful op b/c is modifies the mem queue its reading from
      case IMemRecv(mem: Id, handle: EVar, _: Option[EVar]) =>
        Some(BExprStmt(bsInts.getMemResp(modParams(mem), translator.toVar(handle), cmd.portNum.get, translator.isLockedMem(mem))))
      case IMemWrite(mem, addr, data) =>
        val portNum = mem.typ.get match {
          case memType: TLockedMemType => if (memType.limpl.addWritePort) cmd.portNum else None
          case _ => None
        }
        Some(BExprStmt(
          bsInts.getCombWrite(modParams(mem), translator.toExpr(addr), translator.toExpr(data), portNum, translator.isLockedMem(mem))
        ))
      case ISend(handle, receiver, args) =>
        //Only for sends that are recursive (i.e., not leaving this module)
        if (receiver == mod.name) {
          Some(BStmtSeq(sendToModuleInput(args)))
        } else {
          Some(BInvokeAssign(translator.toVar(handle),
            bsInts.getModRequest(modParams(receiver), args.map(a => translator.toExpr(a)))))
        }
      case IRecv(_, sender, _) =>
        Some(BExprStmt(bsInts.getModResponse(modParams(sender))))
      case COutput(exp) =>
        val outstruct = if (mod.ret.isDefined) {
          BStructLit(outputQueueElem,
            Map(outputStructData -> translator.toExpr(exp),
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
      case cl@IReserveLock(handle, mem) =>
        val methodInfo = LockImplementation.getLockImpl(mem).getReserveInfo(cl)
        if (methodInfo.isDefined) {
          val resMethod = translateMethod(modParams(mem.id), methodInfo.get)
          Some(
            if (methodInfo.get.doesModify) {
              //can't just apply TaggedValid( resMethod) if it is an Action method (i.e., uses <-).
              //Need to assign to a fresh variable and then tag that.
              val handletyp = handle.typ.get.matchOrError(
                handle.pos, "Extract Lock Handle", "Maybe(Handle)") { case TMaybe(t) => t }
              val fresh = freshTmp(translator.toType(handletyp))
              BStmtSeq(List(
                BInvokeAssign(fresh, resMethod).setUseLet(true),
                BAssign(translator.toVar(handle), BTaggedValid(fresh))
              ))
            } else {
              BAssign(translator.toVar(handle), BTaggedValid(resMethod))
          })
        } else {
          throw new RuntimeException(
            s"Lock Library: ${LockImplementation.getLockImpl(mem).toString} has bad Reserve Method"
          )
        }
      case IAssignLock(handle, src, _) => Some(
        BAssign(translator.toVar(handle), translator.toExpr(src))
      )
      case cl@IReleaseLock(mem, h) =>
        val methodInfo = LockImplementation.getLockImpl(mem).getReleaseInfo(cl)
        if (methodInfo.isDefined) {
          Some(
            BExprStmt(translateMethod(modParams(mem.id), methodInfo.get))
          )
        } else { None }
      case CLockStart(mod) => Some(bsInts.getStart(lockRegions(mod)))
      case CLockEnd(mod) => Some(bsInts.getStop(lockRegions(mod)))
      case CPrint(args) => Some(BDisplay(None, args.map(a => translator.toExpr(a))))
      case _: ICheckLockFree => None
      case _: ICheckLockOwned => None
      case _: ILockNoOp => None
      case CSpecCall(handle, _, args) =>
        //send to input
        val sendStmts = sendToModuleInput(args, Some(handle))
        //write to handle (make allocCall)
        val allocExpr = bsInts.getSpecAlloc(specTable)
        val allocAssign = BInvokeAssign(translator.toVar(handle), allocExpr)
        Some(BStmtSeq(allocAssign +: sendStmts))
      case CVerify(handle, args, preds, upd) =>
        val correct = args.zip(preds).foldLeft[BExpr](BBoolLit(true))((b, l) => {
          val a = l._1
          val p = l._2
          BBOp("&&", b, BBOp("==", translator.toExpr(a), translator.toExpr(p)))
        })
        val updCmd = translator.toExpr(upd)
        val specCmd = BIf(correct,
          //true branch, just update spec table
          List(BExprStmt(bsInts.getSpecValidate(specTable, translator.toVar(handle), stgSpecOrder))),
          //false branch, update spec table _and_ resend call with correct arguments
          BExprStmt(bsInts.getSpecInvalidate(specTable, translator.toVar(handle), stgSpecOrder)) +: sendToModuleInput(args)
        )
        Some(if (updCmd.isDefined) {
          BStmtSeq(List(BExprStmt(updCmd.get), specCmd))
        } else {
          specCmd
        })
        //if args != preds -> do spec invalidate and speccall, reassign handle and predictions
      case CUpdate(nh, handle, args, preds) =>
        val incorrect = args.zip(preds).foldLeft[BExpr](BBoolLit(false))((b, l) => {
          val a = l._1
          val p = l._2
          BBOp("||", b, BBOp("!=", translator.toExpr(a), translator.toExpr(p)))
        })
        //order update invalidate AFTER a CInvalidate or CVerify command
        val invalidate =  BExprStmt(bsInts.getSpecInvalidate(specTable, translator.toVar(handle), stgSpecOrder))
        //send to input
        val sendStmts = sendToModuleInput(args, Some(nh))
        //write to handle (make allocCall)
        val allocExpr = bsInts.getSpecAlloc(specTable)
        val allocAssign = BInvokeAssign(translator.toVar(nh), allocExpr)
        //if correct then copy handle over
        val copyHandle = BAssign(translator.toVar(nh), translator.toExpr(handle))
        Some(BIf(incorrect, List(invalidate, allocAssign) ++ sendStmts, List(copyHandle)))
        //Invalidate _doesn't_ resend with correct arguments (since it doesn't know what they are!)
      case CInvalidate(handle) => Some(BExprStmt(
        bsInts.getSpecInvalidate(specTable, translator.toVar(handle), stgSpecOrder)))
        //only free speculation entries for the blocking call
        //but do so conditionally based on being Speculative at all
      case CCheckSpec(isBlocking) if isBlocking =>
        Some(BIf(BIsValid(translator.toBSVVar(specIdVar)),
          List(BExprStmt(bsInts.getSpecFree(specTable, getSpecIdVal))), List()))
      case CCheckSpec(isBlocking) if !isBlocking => None
      case CAssign(_, _) => None
      case CExpr(_) => None
      case CEmpty() => None
      case _: InternalCommand => throw UnexpectedCommand(cmd)
      case CRecv(_, _) => throw UnexpectedCommand(cmd)
      case CSeq(_, _) => throw UnexpectedCommand(cmd)
      case CTBar(_, _) => throw UnexpectedCommand(cmd)
      case CIf(_, _, _) => throw UnexpectedCommand(cmd)
      case CReturn(_) => throw UnexpectedCommand(cmd)
      case CSplit(_, _) => throw UnexpectedCommand(cmd)
      case CLockOp(_, _, _) => throw UnexpectedCommand(cmd)
    }

    private def getRecvCmds(cmds: Iterable[Command]): List[BStatement] = {
      cmds.foldLeft(List[BStatement]())((l, c) => {
        getRecvCmd(c) match {
          case Some(bs) => l :+ bs
          case None => l
        }
      })
    }
    //This is the same as getEffectCmd but only for a small subset of the commands
    private def getRecvCmd(c: Command): Option[BStatement] = c match {
      case ICondCommand(cond: Expr, cs) =>
        val stmtlist = cs.foldLeft(List[BStatement]())((l, c) => {
          getRecvCmd(c) match {
            case Some(bc) => l :+ bc
            case None => l
          }
        })
        if (stmtlist.nonEmpty) Some(BIf(translator.toExpr(cond), stmtlist, List())) else None
      //This is an effectful op b/c is modifies the mem queue its reading from
      case IMemRecv(mem: Id, handle: EVar, _: Option[EVar]) =>
        Some(BExprStmt(
          bsInts.getMemResp(modParams(mem), translator.toVar(handle), c.portNum.get, translator.isLockedMem(mem))))
      case IRecv(_, sender, _) =>
        Some(BExprStmt(bsInts.getModResponse(modParams(sender))))
      case _ => None
    }
    private def sendToModuleInput(args: List[Expr], specHandle: Option[EVar] = None) = {
      //need to add threadid and specid vars explicitly, its not in the surface syntax
      var argmap = args.map(a => translator.toExpr(a)) :+ translator.toBSVVar(threadIdVar)
      if (mod.maybeSpec) {
        argmap = argmap :+ (if (specHandle.isDefined) { BTaggedValid(translator.toVar(specHandle.get)) } else { BInvalid })
      }
      getEdgeQueueStmts(firstStage.inEdges.head.from, firstStage.inEdges, Some(argmap))
    }

  }

}
