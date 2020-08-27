package pipedsl.codegen

import pipedsl.common.BSVSyntax
import pipedsl.common.BSVSyntax._
import pipedsl.common.DAGSyntax.{PStage, PipelineEdge}
import pipedsl.common.Errors.{IllegalBSVStage, UnexpectedBSVType, UnexpectedCommand}
import pipedsl.common.Syntax._

//TODO turn this into a proper class with members
//so that we don't have to pass lots of structural info through method parameters.
object BluespecGeneration {

  private val wireType = "Wire"
  private val wireModuleName = "mkWire"
  private val fifoType = "FIFOF"
  private val fifoModuleName = "mkFIFOF"

  private val memType = "BRAM"
  private val memModuleName = "mkBRAM1Server"

  type EdgeInfo = Map[PipelineEdge, BStructDef]
  type ModInfo = Map[Id, BVar]
  //Normal Map that special cases when checking the first Stage as part of the key
  //Since The First stage only has 1 incoming 'edge'
  class EdgeMap(firstStage: PStage, firstStageInput: BStruct, edges: EdgeInfo) {

    private val realMap: Map[PipelineEdge, BStruct] = edges map { case (k, v) => (k, v.typ) }

    def apply(e: PipelineEdge): BStruct = {
      realMap.get(e) match {
        case Some(b) => b
        case None if e.to == firstStage => firstStageInput
        case _ => throw new RuntimeException("Missing edge struct type")
      }
    }

  }

  type StageTypes = Map[PStage, BInterfaceDef]

  /**
   * Uses the configured fifo interface type and the provided
   * BSV type to make a paramterized fifo type.
   * @param typ - The BSV type that describes the fifo's elements' types
   * @return - The new BSV type describing the parameterized fifo
   */
  def getFifoType(typ: BSVType): BInterface = {
    BInterface(fifoType, List(BVar("elemtyp", typ)))
  }

  /**
   * Generate a Wire type that holds the given BSVType.
   * Wires allow passing data between rules on the same cycle.
   * @param typ - The BSV type that describes the wire's content type
   * @return - The new BSV type describing the parameterized Wire
   */
  def getWireType(typ: BSVType): BInterface = {
    BInterface(wireType, List(BVar("elemtyp", typ)))
  }
  //TODO generate a struct literal that represents passing parameters to an external modules
  //only works for memories right now.
  def getRequestStruct(modTyp: BSVType, sendreq: Command): BStructLit = modTyp match {
    case t@BMemType(_, _) => sendreq match { case c: IMemSend => getMemRequestStruct(t, c) }
    case BInterface(name, tparams) => //TODO
      throw UnexpectedBSVType(s"Type $modTyp does not receive requests")
    case _ => throw UnexpectedBSVType(s"Type $modTyp does not receive requests")
  }

  /**
   * Given a memory request, generates the corresponding
   * memory request struct, according to the compiler's configured memory type
   * and the type of memory the request is for.
   * @param typ - The memory type that the request is targeting
   * @param sendreq - The memory send operation
   * @return - The struct representing the request to the appropriate type of memory.
   */
  def getMemRequestStruct(typ: BMemType, sendreq: IMemSend): BStructLit = {
    val fields: Map[BVar, BExpr] = Map(
      BVar("write", BBool) -> BBoolLit(sendreq.isWrite),
      BVar("responseOnWrite", BBool) -> BBoolLit(false),
      BVar("address", BSizedInt(unsigned = true, typ.addrSize)) -> toBSVExpr(sendreq.addr),
      BVar("datain", typ.elem) -> (sendreq.data match {
        case Some(din) => toBSVExpr(din)
        case None => BDontCare
      })
    )
    val reqType = BStruct("BRAMRequest", fields.keys.toList)
    BStructLit(reqType, fields)
  }

  /**
   * Given a list of pipeline stages that describe a pipeline module
   * and the interface to that module, generate a
   * BSV Module that represents the entire pipeline.
   *
   * This comment and top-level function are still TODO,
   * we need to refactor a bit.
   * @param firstStage - The first stage in the pipeline that accepts inputs from
   *                   a single channel (unlike the other stages).
   * @param rest - The full remaining list of pipeline stages.
   * @return - The BSV Module that represents this pipeline.
   */
  def getBSV(mod: ModuleDef, firstStage: PStage, rest: List[PStage]): BProgram = {
    //Data types for passing between stages
    val edgeStructInfo = getEdgeStructInfo(firstStage +: rest)
    //First stage should have exactly one input edge by definition
    val firstStageStruct = edgeStructInfo(firstStage.inEdges.head)
    //This map returns the correct struct type based on the destination stage
    val completeEdgeMap = new EdgeMap(firstStage, firstStageStruct.typ, edgeStructInfo)
    //Generate map from existing module parameter names to BSV variables
    val modParams: ModInfo = mod.modules.foldLeft[ModInfo](Map())((vars, m) => {
      vars + (m.name -> BVar(m.name.v, toBSVType(m.typ)))
    })
    //Generate a Submodule for each stage
    val stgMap = (firstStage +: rest).foldLeft(Map[PStage, BModuleDef]())((m, s) => {
      m + (s -> getStageModule(s, completeEdgeMap, modParams))
    })
    val structDefs = edgeStructInfo.values.toList
    //TODO define the top level module interface based on the declaration
    //this requires modifying the parameters to this function (getBSV)
    BProgram(topModule = getTopModule(firstStage, rest, completeEdgeMap, stgMap, modParams),
      imports = List(BImport(fifoType)), structs = structDefs, interfaces = List(), modules = stgMap.values.toList)
  }

  /**
   * Converts the edge value information into BSV structs
   * that hold all of those values and puts it into a lookup structure.
   * @param stgs - The list of stages to process.
   * @return - A map from edges to the BSV type definitions that represent their values.
   */
  private def getEdgeStructInfo(stgs: Iterable[PStage]): Map[PipelineEdge, BStructDef] = {
    stgs.foldLeft[Map[PipelineEdge, BStructDef]](Map())((m, s) => {
      s.inEdges.foldLeft[Map[PipelineEdge, BStructDef]](m)((ms, e) => {
        ms + (e -> getEdgeStruct(e))
      })
    })
  }
  //A Helper for @getEdgeStructInfo
  private def getEdgeStruct(e: PipelineEdge): BStructDef = {
    val styp = BStruct(genEdgeName(e), getBsvStructFields(e.values))
    BStructDef(styp, List("Bits", "Eq"))
  }
  //A Helper for @getEdgeStruct
  private def getBsvStructFields(inputs: Iterable[Id]): List[BVar] = {
    inputs.foldLeft(List[BVar]())((l, id) => {
      l :+ BVar(id.v, toBSVType(id.typ.get))
    })
  }

  //Helper functions that cannonically generate names from pipeline structures.
  private def genEdgeName(e: PipelineEdge) = {
    "E_" + e.from.name.v + "_TO_" + e.to.name.v
  }
  private def genParamName(e: PipelineEdge): String = {
    "fifo_" + e.from.name.v + "_" + e.to.name.v
  }
  private def genParamName(s: PStage): String = {
    "s_" + s.name
  }
  private def genModuleName(s: PStage): String = {
    "mk" + s.name.v
  }

  /**
   * Given a pipeline stage and the necessary edge info,
   * generate a BSV module definition.
   * @param stg - The pipeline stage to convert
   * @param edgeMap - A lookup table from pipeline edges to BSV Structs
   * @return - The generated BSV module definition
   */
  private def getStageModule(stg: PStage, edgeMap: EdgeMap, mods: ModInfo): BModuleDef = {
    //Define module parameters to communicate along pipeline edges
    //TODO also generate the memory parameters we expect to read from/write to
    val outMap = stg.outEdges.foldLeft[Map[PipelineEdge, BVar]](Map())((m, e) => {
      val edgeStructType = edgeMap(e)
      m + (e -> BVar(genParamName(e.to), getFifoType(edgeStructType)))
    })
    val inOutMap = outMap ++ stg.inEdges.foldLeft[Map[PipelineEdge, BVar]](Map())((m, e) => {
      val edgeStructType = edgeMap(e)
      m + (e -> BVar(genParamName(e.from), getFifoType(edgeStructType)))
    })
    //TODO only pass the module parameters that are actually needed
    val params = inOutMap.values.toList ++ mods.values
    //Generate the combinational connections and Wire definitions for communicating between rules
    val sBody = getStageBody(stg, inOutMap, edgeMap)
    //Generate the set of execution rules for reading args and writing outputs
    val execRules: List[BRuleDef] = getExecRules(stg, edgeMap, inOutMap, mods)
    //TODO We don't need to generate methods at the moment since all communication is via FIFOs
    // this may need to change eventually
    BModuleDef(name = genModuleName(stg), typ = None,
      params = params, body = sBody, rules = execRules, methods = List())
  }

  //TODO - conditional recv/sends assume mutual exclusion at the moment, we need
  //to ensure that the compiler makes this always true.
  /**
   * Given a pipeline stage, generate the list of execution rules
   * that correspond to reading inputs and writing outputs. The generated
   * rules guarantee that reads and writes always happen in the same cycle,
   * even if they are different rules.
   * @param stg - The pipeline stage to process
   * @param edgeMap - The edge to BSV type lookup info
   * @param paramMap - The edge to module param info (a.k.a which variable represents the edge fifo)
   * @return - The list of BSV execution rules
   */
  private def getExecRules(stg: PStage, edgeMap: EdgeMap, paramMap: Map[PipelineEdge, BVar], mods: ModInfo): List[BRuleDef] = {
    val writeRules = getWriteRules(stg, edgeMap, paramMap, mods)
    val readRules = getReadRules(stg, edgeMap, paramMap, mods)
    val condReads = readRules.filter(r => r.conds.nonEmpty)
    val condWrites = writeRules.filter(r => r.conds.nonEmpty)
    val execRules: List[BRuleDef] = (condReads.nonEmpty, condWrites.nonEmpty) match {
      case (true, true) => throw IllegalBSVStage("Cannot conditionally read & write in a single stage")
      case (true, false) => {
        //has conditional inputs but no conditional outputs
        //1 rule for each cond read, and 1 write rule
        val outRule = combineRules("execOutput", writeRules)
        val implicitOutConds = getImplicitOutputConditions(stg, edgeMap, paramMap)
        outRule +: condReads.map(r => {
          BRuleDef(r.name, r.conds ++ implicitOutConds, r.body)
        })
      }
      case (false, true) => {
        //has conditional outputs but not inputs
        //one rule for each cond write which also triggers the read rules
        condWrites.foldLeft(List[BRuleDef]())((l, r) => {
          val outRule = combineRules(r.name, readRules :+ r)
          l :+ outRule
        })
      }
      case (false, false) => {
        //Single execute rule to do everything
        List(combineRules("execute", readRules ++ writeRules))
      }
    }
    execRules
  }
  /**
   * Given a stage and the relevant edge information, generate the
   * write rules for the stage which involve writing data to the output
   * edges and also any output modules (memories, etc.)
   * @param stg - The stage to process
   * @param edgeMap - Edge to Struct Type Mapping
   * @param paramMap - Edge to Parameter name Mapping
   * @return A list of BSV rules
   */
  private def getWriteRules(stg: PStage, edgeMap: EdgeMap, paramMap: Map[PipelineEdge, BVar], mods: ModInfo): List[BRuleDef] = {
    val (condOut, uncondOut) = stg.outEdges.partition(e => e.condSend.isDefined)
    val uncondOutStmts = getEdgeStatements(stg, uncondOut, edgeMap, paramMap, readInputWires = false)
    val writeCmdStmts = getWriteCmds(stg.cmds, mods)
    if (condOut.isEmpty) {
      List(BRuleDef("writeOutputs", List(), uncondOutStmts ++ writeCmdStmts))
    } else {
      condOut.foldLeft(List[BRuleDef]())((l, e) => {
        val outStmts = getEdgeStatements(stg, List(e), edgeMap, paramMap, readInputWires = false) ++ uncondOutStmts ++ writeCmdStmts
        //each rule has the write stmts for that edge, and all the unconditional writes
        l :+ BRuleDef("input_" + l.size, List(toBSVExpr(e.condSend.get)), outStmts)
      })
    }
  }
  /**
   * Given a stage and the relevant edge information, generate the
   * read rules for the stage which involve reading data from the input
   * edges and also any input modules (memories, etc.)
   * @param stg - The stage to process
   * @param edgeMap - Edge to Struct Type Mapping
   * @param paramMap - Edge to Parameter name Mapping
   * @return A list of BSV rules
   */
  private def getReadRules(stg: PStage, edgeMap: EdgeMap, paramMap: Map[PipelineEdge, BVar], mods: ModInfo): List[BRuleDef] = {
    val (condIn, uncondIn) = stg.inEdges.partition(e => e.condRecv.isDefined)
    val uncondInStmts = getEdgeStatements(stg, uncondIn, edgeMap, paramMap, readInputWires = false)
    val readCmdStmts = getReadCmds(stg.cmds, mods)
    if (condIn.isEmpty) {
      List(BRuleDef("readInputs", List(), uncondInStmts ++ readCmdStmts))
    } else {
      condIn.foldLeft(List[BRuleDef]())((l, e) => {
        val inStmts = getEdgeStatements(stg, List(e), edgeMap, paramMap, readInputWires = true) ++ uncondInStmts ++ readCmdStmts
        //each rule has the read stmts for that edge, and all the unconditional reads
        l :+ BRuleDef("input_" + l.size, List(toBSVExpr(e.condRecv.get)), inStmts)
      })
    }
  }
  /**
   *
   * @param s
   * @param es
   * @param edgeMap
   * @param paramMap
   * @param readInputWires
   * @return
   */
  private def getEdgeStatements(s: PStage, es: Iterable[PipelineEdge],
    edgeMap: EdgeMap, paramMap: Map[PipelineEdge, BVar], readInputWires: Boolean = false): List[BStatement] = {
    es.foldLeft(List[BStatement]())((l, e) => {
      val stmts = if (e.to == s) {
        val wireAssigns: List[BStatement] = if (readInputWires) {
          e.values.foldLeft(List[BStatement]())((le, v) => {
            val paramExpr = BMethodInvoke(paramMap(e), "first", List())
            le :+ BModAssign(
              BVar(v.v, getWireType(toBSVType(v.typ.get))),
              BStructAccess(paramExpr, BVar(v.v, toBSVType(v.typ.get))))
          })
        } else {
          List[BStatement]()
        }
        wireAssigns :+ BExprStmt(BMethodInvoke(paramMap(e), "deq", List()))
      } else {
        val op = getCanonicalStruct(edgeMap(e))
        List(BExprStmt(BMethodInvoke(paramMap(e), "enq", List(op))))
      }
      l ++ stmts
    })
  }

  /**
   *
   * @param s
   * @param edgeMap
   * @param paramMap
   * @return
   */
  private def getImplicitOutputConditions(s: PStage, edgeMap: EdgeMap, paramMap: Map[PipelineEdge, BVar]): List[BExpr] = {
    getEdgeConditions(s, s.outEdges, edgeMap, paramMap)
    //TODO also get the conditions from readStatements
  }
  /**
   *
   * @param s
   * @param es
   * @param edgeMap
   * @param paramMap
   * @return
   */
  private def getEdgeConditions(s: PStage, es: Iterable[PipelineEdge],
    edgeMap: EdgeMap, paramMap: Map[PipelineEdge, BVar]): List[BExpr] = {
    es.foldLeft(List[BExpr]())((l, e) => {
      val stmt = if (e.to == s) {
        BMethodInvoke(paramMap(e), "notEmpty", List())
      } else {
        BMethodInvoke(paramMap(e), "notFull", List())
      }
      l :+ stmt
    })
  }

  private def getStageBody(stg: PStage, pmap: Map[PipelineEdge, BVar], edgeMap: EdgeMap): List[BStatement] = {
    //First define all of the variables read by some edge
    val (condIn, uncondIn) = stg.inEdges.partition(e => e.condRecv.isDefined)
    var body: List[BStatement] = List()
    //unconditional reads are just variable declarations w/ values
    uncondIn.foreach(e => {
      //First element in read queue
      val paramExpr = BMethodInvoke(pmap(e), "first", List())
      e.values.foreach(v => {
        body = body :+ BDecl(BVar(v.v, toBSVType(v.typ.get)),
          BStructAccess(paramExpr, BVar(v.v, toBSVType(v.typ.get))))
      })
    })
    //conditional reads are Wires - get a set of values from all inputs to avoid double declaration
    condIn.foldLeft(Set[Id]())((s, e) => {
      s ++ e.values
    }).foreach(v => {
      body = body :+ BModInst(lhs = BVar(v.v, getWireType(toBSVType(v.typ.get))),
        rhs = BModule(wireModuleName, List()))
    })
    body = body ++ getCombinationalCommands(stg.cmds)
    body
  }
  //TODO add in the module parameters into the definition w/ appropriate types
  private def getTopModule(firstStg: PStage, otherStgs: List[PStage], edgeMap: EdgeMap, stgMap: Map[PStage, BModuleDef], mods: ModInfo): BModuleDef = {
    //Body instantiates all of the params (fifos & memories) and then all of the stages
    //One fifo per edge in the graph
    val allEdges = (firstStg +: otherStgs).foldLeft(Set[PipelineEdge]())((es, s) => {
      es ++ s.inEdges ++ s.outEdges
    })
    val edgeFifos = allEdges.foldLeft(Map[PipelineEdge, BModInst]())((m, e) => {
      m + (e -> BModInst(BVar(genParamName(e), edgeMap(e)), BModule(fifoModuleName, List())))
    })
    val mkStgs = (firstStg +: otherStgs).map(s => {
      val moddef = stgMap(s)
      val modtyp = moddef.typ match {
        case Some(t) => t
        case None => BEmptyModule
      }
      val args = s.allEdges.map(e => {
        edgeFifos(e).lhs
      }).toList ++ mods.values.toList //TODO only pass mods that are necessary
      BModInst(BVar(genParamName(s), modtyp), BModule(moddef.name, args))
    })
    val stmts = edgeFifos.values.toList ++ mkStgs
    //TODO Execute rule is an unconditional send to the first stage
    BModuleDef(name = "mkTop", typ = None, params = mods.values.toList, body = stmts,
      rules = List(), methods = List())
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
   *
   * @param cmd
   * @return
   */
  private def getCombinationalCommand(cmd: Command): Option[BStatement] = cmd match {
    case CAssign(lhs, rhs) =>
      Some(BDecl(BVar(lhs.id.v, toBSVType(lhs.typ.get)), toBSVExpr(rhs)))
    case ICondCommand(cond: Expr, c: Command) => getCombinationalCommand(c) match {
      case Some(bc) => Some(BIf(toBSVExpr(cond), List(bc), List()))
      case None => None
    }
    case CExpr(exp) => Some(BExprStmt(toBSVExpr(exp)))
    case CCall(id, args) => None
    case CLockOp(mem, op) => None
    case CCheck(predVar) => None
    case CEmpty => None
    case v: ISpeculate => None
    case v: IUpdate => None
    case v: ICheck => None
    case IMemRecv(_, data) => data match {
      case Some(d) => Some(BModInst(lhs = BVar(d.id.v, getWireType(toBSVType(d.typ.get))),
        rhs = BModule(wireModuleName, List())))
      case None => None
    }
    case v: IMemSend => None
    case v: ISend => None
    case v: IRecv => None
    case CRecv(lhs, rhs) => throw UnexpectedCommand(cmd)
    case CIf(cond, cons, alt) => throw UnexpectedCommand(cmd)
    case CSeq(c1, c2) => throw UnexpectedCommand(cmd)
    case CTBar(c1, c2) => throw UnexpectedCommand(cmd)
    case COutput(exp) => throw UnexpectedCommand(cmd)
    case CReturn(exp) => throw UnexpectedCommand(cmd)
    case CSpeculate(predVar, predVal, verify, body) => throw UnexpectedCommand(cmd)
    case CSplit(cases, default) => throw UnexpectedCommand(cmd)
  }

  //Helper to accumulate getReadCmd results into a single list
  private def getReadCmds(cmds: Iterable[Command], mods: ModInfo): List[BStatement] = {
    cmds.foldLeft(List[BStatement]())((l, c) => {
      getReadCmd(c, mods) match {
        case Some(bs) => l :+ bs
        case None => l
      }
    })
  }
  /**
   * If this command represents a statement that must be executed during
   * the stage's read rules, translate and return the translation, otherwise
   * return None.
   * Currently, the only read command is IMemRecv.
   * @param cmd - The command to translate
   * @return Some(translated statement) or None if not a read command
   */
  private def getReadCmd(cmd: Command, mods: ModInfo): Option[BStatement] = cmd match {
    case ICondCommand(cond: Expr, c: Command) => getReadCmd(c, mods) match {
      case Some(bc) => Some(BIf(toBSVExpr(cond), List(bc), List()))
      case None => None
    }
    case IMemSend(_: Boolean, _: Id, _: Option[EVar], _: EVar) => None
    case IMemRecv(mem: Id, data: Option[EVar]) => data match {
      case Some(v) => {
        val tmpVar = BVar("_" + mem.v + "_resp_" + v.id.v, toBSVType(v.typ.get))
        //TODO make configurable and figure out how to determine the port,
        //if multiple ports are supported
        val getPort = BMethodInvoke(mods(mem), "portA", List())
        val getResp = BMethodInvoke(getPort, "response", List())
        val assignTmp = BInvokeAssign(tmpVar, BMethodInvoke(getResp, "get", List()))
        val assignFinal = BModAssign(BVar(v.id.v, getWireType(toBSVType(v.typ.get))), tmpVar)
        Some(BStmtSeq(List(assignTmp, assignFinal)))
      }
      case None => None
    }
    //TODO implement locks
    case CLockOp(mem, op) => None
    case CCall(id, args) => None
    case CAssign(lhs, rhs) => None
    case CExpr(exp) => None
    case CEmpty => None
    case command: InternalCommand => throw UnexpectedCommand(cmd)
    case CCheck(predVar) => throw UnexpectedCommand(cmd)
    case CRecv(lhs, rhs) => throw UnexpectedCommand(cmd)
    case CSeq(c1, c2) => throw UnexpectedCommand(cmd)
    case CTBar(c1, c2) => throw UnexpectedCommand(cmd)
    case CIf(cond, cons, alt) => throw UnexpectedCommand(cmd)
    case COutput(exp) => throw UnexpectedCommand(cmd)
    case CReturn(exp) => throw UnexpectedCommand(cmd)
    case CSpeculate(predVar, predVal, verify, body) => throw UnexpectedCommand(cmd)
    case CSplit(cases, default) => throw UnexpectedCommand(cmd)
  }

  //Helper to accumulate getWriteCmd results into a single list
  private def getWriteCmds(cmds: Iterable[Command], mods: ModInfo): List[BStatement] = {
    cmds.foldLeft(List[BStatement]())((l, c) => {
      getWriteCmd(c, mods) match {
        case Some(bs) => l :+ bs
        case None => l
      }
    })
  }
  /**
   *
   * @param mods
   * @param cmd
   * @return
   */
  private def getWriteCmd(cmd: Command, mods: ModInfo): Option[BStatement] = cmd match {
    case ICondCommand(cond: Expr, c: Command) => getWriteCmd(c, mods) match {
      case Some(bc) => Some(BIf(toBSVExpr(cond), List(bc), List()))
      case None => None
    }
    //TODO implement locks
    case CLockOp(mem, op) => None
    //TODO no magic variables, tie to memory implementation configuration
    case c@IMemSend(_: Boolean, mem: Id, _: Option[EVar], _: EVar) => {
      val request = getRequestStruct(mods(mem).typ, c)
      val getPort = BMethodInvoke(mods(mem), "portA", List())
      val getReq = BMethodInvoke(getPort, "request", List())
      val putReq = BMethodInvoke(getReq, "put", List(request))
      Some(BExprStmt(putReq))
    }
    //TODO implement calls
    case CCall(id, args) => None
    case IMemRecv(_: Id, _: Option[EVar]) => None
    case CAssign(lhs, rhs) => None
    case CExpr(exp) => None
    case CEmpty => None
    case command: InternalCommand => throw UnexpectedCommand(cmd)
    case CCheck(predVar) => throw UnexpectedCommand(cmd)
    case CRecv(lhs, rhs) => throw UnexpectedCommand(cmd)
    case CSeq(c1, c2) => throw UnexpectedCommand(cmd)
    case CTBar(c1, c2) => throw UnexpectedCommand(cmd)
    case CIf(cond, cons, alt) => throw UnexpectedCommand(cmd)
    case COutput(exp) => throw UnexpectedCommand(cmd)
    case CReturn(exp) => throw UnexpectedCommand(cmd)
    case CSpeculate(predVar, predVal, verify, body) => throw UnexpectedCommand(cmd)
    case CSplit(cases, default) => throw UnexpectedCommand(cmd)
  }

}