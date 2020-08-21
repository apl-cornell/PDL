package pipedsl.passes

import pipedsl.common.BSVPrettyPrinter._
import pipedsl.common.BSVSyntax._
import pipedsl.common.DAGSyntax._
import pipedsl.common.Errors.{IllegalBSVStage, MissingType, UnexpectedCommand, UnexpectedExpr}
import pipedsl.common.Syntax
import pipedsl.common.Syntax._
import pipedsl.common.Utilities._
import pprint.pprintln


object BluespecGeneration {

  val wireType = "Wire"
  val wireModuleName = "mkWire"
  val fifoType = "FIFOF"
  val fifoModuleName = "mkFIFOF"
  val firstStageIntName = "S_start"
  val firstStageStructName = "E_start"
  val firstStageName = "start"

  type EdgeInfo = Map[PipelineEdge, BStructDef]
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

  def getFifoType(typ: BSVType): BInterface = {
    BInterface(fifoType, List(BVar("elemtyp", typ)))
  }

  def getWireType(typ: BSVType): BInterface = {
    BInterface(wireType, List(BVar("elemtyp", typ)))
  }

  def getFifoType(t: String): String = {
    fifoType + " #(" + t +")"
  }

  def getBSV(firstStage: PStage, inputs: List[Id], rest: List[PStage]): BProgram = {
    //Data types for passing between stages
    val firstStageStruct = getFirstStageStruct(inputs)
    val edgeStructInfo = getEdgeStructInfo(rest)
    val completeEdgeMap = new EdgeMap(firstStage, firstStageStruct.typ, edgeStructInfo)

    //Interfaces exposed by each stage
    val firstStageInterface = getFirstStageInterface(firstStageStruct.typ)
    val stageInterfacemap = getStageIntMap(rest, edgeStructInfo) + (firstStage -> firstStageInterface)

    //Module for each stage
    val otherModules = rest.foldLeft(List[BModuleDef]())((l, s) => {
      l :+ getStageModule(s, stageInterfacemap, completeEdgeMap)
    })
    val structDefs = firstStageStruct +: edgeStructInfo.values.toList
    val intDefs = stageInterfacemap.values.toList
    BProgram(topModule = "mkTop", imports = List(), structs = structDefs, interfaces = intDefs, modules = otherModules)
  }

  private def getFirstStageStruct(inputs: List[Id]): BStructDef = {
    val styp = BStruct(firstStageStructName, getBsvStructFields(inputs))
    BStructDef(styp, List("Bits", "Eq"))
  }

  private def getFirstStageInterface(struct: BSVType): BInterfaceDef = {
    BInterfaceDef(BInterface(firstStageIntName),
      List(BMethodSig(firstStageName, MethodType.Action, List(BVar("d_in",struct)))))
  }

  private def getEdgeStructInfo(stgs: List[PStage]): Map[PipelineEdge, BStructDef] = {
    stgs.foldLeft[Map[PipelineEdge, BStructDef]](Map())((m, s) => {
      s.inEdges.foldLeft[Map[PipelineEdge, BStructDef]](m)((ms, e) => {
        ms + (e -> getEdgeStruct(e))
      })
    })
  }

  private def getEdgeStruct(e: PipelineEdge): BStructDef = {
    val styp = BStruct("E_" + e.from.name.v + "-" + e.to.name.v, getBsvStructFields(e.values))
    BStructDef(styp, List("Bits", "Eq"))
  }

  private def getBsvStructFields(inputs: Iterable[Id]): List[BVar] = {
    inputs.foldLeft(List[BVar]())((l, id) => {
      l :+ BVar(id.v, toBSVType(id.typ.get))
    })
  }

  private def getStageIntMap(stgs: List[PStage], edgeTypes: EdgeInfo): StageTypes = {
    stgs.foldLeft[StageTypes](Map())((m, s) => {
      m + (s -> getStageInterface(s, edgeTypes))
    })
  }

  private def getStageInterface(stg: PStage, edgeTypes: EdgeInfo): BInterfaceDef = {
    val methodSigs = stg.inEdges.foldLeft[List[BMethodSig]](List())((l, e) => {
      l :+ BMethodSig(getSendName(e), MethodType.Action, List(BVar("d_in", edgeTypes(e).typ)))
    })
    BInterfaceDef(BInterface("S_" + stg.name.v), methodSigs)
  }

  private def genParamName(s: PStage): String = {
    "s_" + s.name
  }

  private def genModuleName(s: PStage): String = {
    "mk" + s.name.v
  }

  def getStageModule(stg: PStage, stageTypes: StageTypes, edgeMap: EdgeMap): BModuleDef = {
    //Define params (fifos which transmit data to and from this stage)
    val outMap = stg.outEdges.foldLeft[Map[PipelineEdge, BVar]](Map())((m, e) => {
      val edgeStructType = edgeMap(e)
      m + (e -> BVar(genParamName(e.to), getFifoType(edgeStructType)))
    })
    val paramMap = outMap ++ stg.inEdges.foldLeft[Map[PipelineEdge, BVar]](Map())((m, e) => {
      val edgeStructType = edgeMap(e)
      m + (e -> BVar(genParamName(e.from), getFifoType(edgeStructType)))
    })
    //TODO handle the first stage too since it doesn't have any incoming edges
    //and should only have 1 input FIFO

    //Generate the combinational connections
    val sBody = getStageBody(stg, paramMap, edgeMap)
    //Generate the set of execution rules for reading args and writing outputs
    val execRules: scala.List[BRuleDef] = getExecRules(stg, edgeMap, paramMap)
    //No methods at the moment since all communication is via FIFOs
    //TODO this will probably need to change eventually
    BModuleDef(name = genModuleName(stg), typ = stageTypes(stg).typ,
      params = paramMap.values.toList, body = sBody, rules = execRules, methods = List())
  }

  //TODO - conditional recv/sends assume mutual exclusion at the moment
  private def getExecRules(stg: PStage, edgeMap: EdgeMap, paramMap: Map[PipelineEdge, BVar]): List[BRuleDef] = {
    val (condIn, uncondIn) = stg.inEdges.partition(e => e.condRecv.isDefined)
    val (condOut, uncondOut) = stg.outEdges.partition(e => e.condSend.isDefined)
    val execRules: List[BRuleDef] = (condIn.nonEmpty, condOut.nonEmpty) match {
      case (true, true) => throw IllegalBSVStage("Cannot conditionally read & write in a single stage")
      case (true, false) => {
        //has conditional inputs but no conditional outputs
        //1 rule for each cond exec, single output execute rule
        val outstmts = getEdgeStatements(stg, stg.outEdges, edgeMap, paramMap)
        val outconds = getEdgeConditions(stg, stg.outEdges, edgeMap, paramMap)
        val outRule = BRuleDef("execute", List(), outstmts)
        val uncondInStmts = getEdgeStatements(stg, uncondIn, edgeMap, paramMap)
        val inRules = condIn.foldLeft(List[BRuleDef]())((l, e) => {
          val inStmts = getEdgeStatements(stg, List(e), edgeMap, paramMap) ++ uncondInStmts
          //each rule has the read stmts for that edge, all the unconditional reads
          l :+ BRuleDef("input_" + l.size, outconds :+ toBSVExpr(e.condRecv.get), inStmts)
        })
        outRule +: inRules
      }
      case (false, true) => {
        //has conditional outputs but not inputs
        val uncondStatements = getEdgeStatements(stg, uncondIn ++ uncondOut, edgeMap, paramMap)
        //one rule for each cond out which also triggers the unconditional edges
        condOut.foldLeft(List[BRuleDef]())((l, e) => {
          val outstmt = getEdgeStatements(stg, List(e), edgeMap, paramMap)
          val newRule = BRuleDef("exec_" + l.size,
            List(toBSVExpr(e.condSend.get)), uncondStatements ++ outstmt)
          l :+ newRule
        })
      }
      case (false, false) => {
        //Single execute rule to do everything
        val allstmts = getEdgeStatements(stg, stg.allEdges, edgeMap, paramMap)
        List(BRuleDef("execute", List(), allstmts))
      }
    }
    execRules
  }

  private def getEdgeStatements(s: PStage, es: Iterable[PipelineEdge],
    edgeMap: EdgeMap, paramMap: Map[PipelineEdge, BVar]): List[BStatement] = {
    es.foldLeft(List[BStatement]())((l, e) => {
      val stmt = if (e.to == s) {
        BExprStmt(BMethodInvoke(paramMap(e), "deq", List()))
      } else {
        val op = getCanonicalStruct(edgeMap(e))
        BExprStmt(BMethodInvoke(paramMap(e), "enq", List(op)))
      }
      l :+ stmt
    })
  }

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
        body = body :+ BAssign(BVar(v.v, toBSVType(v.typ.get)),
          BStructAccess(paramExpr, BVar(v.v, toBSVType(v.typ.get))))
      })
    })
    //conditional reads are Wires
    condIn.foreach(e => {
      //First element in read queue
      val paramExpr = BMethodInvoke(pmap(e), "first", List())
      e.values.foreach(v => {
        body = body :+ BModInst(lhs = BVar(v.v, getWireType(toBSVType(v.typ.get))),
          rhs = BModule(wireModuleName, List()))
      })
    })
    body = body ++ getCombinationalCommands(stg.cmds)
    body
  }

  private def getCombinationalCommands(cmds: List[Command]): List[BStatement] = {
    cmds.foldLeft(List[BStatement]())((l, c) => {
      getCombinationalCommand(c) match {
        case Some(bs) => l :+ bs
        case None => l
      }
    })
  }
  private def getCombinationalCommand(cmd: Command): Option[BStatement] = cmd match {
    case CAssign(lhs, rhs) =>
      Some(BAssign(BVar(lhs.id.v, toBSVType(lhs.typ.get)), toBSVExpr(rhs)))
    case ICondCommand(cond: Expr, c: Command) =>
      Some(BIf(toBSVExpr(cond), List(getCombinationalCommand(c).get), List()))
    case CExpr(exp) => Some(BExprStmt(toBSVExpr(exp)))
    case CRecv(lhs, rhs) => None
    case CCall(id, args) => None
    case CLockOp(mem, op) => None
    case CCheck(predVar) => None
    case Syntax.CEmpty => None
    case v: ISpeculate => None
    case v: IUpdate => None
    case v: ICheck => None
    case CIf(cond, cons, alt) => throw UnexpectedCommand(cmd)
    case CSeq(c1, c2) => throw UnexpectedCommand(cmd)
    case CTBar(c1, c2) => throw UnexpectedCommand(cmd)
    case COutput(exp) => throw UnexpectedCommand(cmd)
    case CReturn(exp) => throw UnexpectedCommand(cmd)
    case CSpeculate(predVar, predVal, verify, body) => throw UnexpectedCommand(cmd)
    case CSplit(cases, default) => throw UnexpectedCommand(cmd)
  }

  def run(firstStage: PStage, inputs: List[Param], rest: List[PStage]): String = {
    pprintln(genFirstStageDefs(firstStage, inputs))
    rest.foreach(stg => {
      pprintln(genEdgeStruct(stg))
    })
    rest.foreach(stg => {
      pprintln(genStageInterface(stg))
    })
    pprintln(genFirstStageModule(firstStage, inputs))
    rest.foreach(stg => {
      pprintln(genStageModule(stg))
    })
    ""
  }

  def getInputName(s: PStage): String = {
    "E_" + s.name
  }
  def getEdgeName(e: PipelineEdge): String = {
    "E_" + e.from.name + "-" + e.to.name
  }

  def genStructMembers(inputs: Iterable[Id]): String = {
    var result = ""
    inputs.foreach(id => {
      result += printBSVType(id.typ.getOrThrow(MissingType(id.pos, id.v))) +
        " " + id.v + "; "
    })
    result
  }

  def genEdgeStruct(s: PStage): String = {
    s.outEdges.foldLeft("")((str, e) => {
      var result = "typedef struct { ";
      result += genStructMembers(e.values)
      result += "} " + getEdgeName(e) + " deriving(Bits, Eq);\n"
      str + result
    })
  }

  def getSendName(e: PipelineEdge): String = {
    e.from.name + "To" + e.to.name
  }

  def getEdgeParam(): String = {
    "d_in"
  }

  def genStageInterface(s: PStage): String = {
    var result = "interface S_" + s.name + ";\n"
    s.inEdges.foreach(e => {
      result += "  method Action " + getSendName(e) + "(" + getEdgeName(e) +
        " " + getEdgeParam() + ");\n"
    })
    result += "endinterface\n"
    result
  }

  def getInterfaceName(s: PStage): String = {
    "S_" + s.name
  }

  def getInputParam(e: PipelineEdge): String = {
    "input_" + e.from.name
  }

  def genFirstStageDefs(s: PStage, inputs: List[Param]): String = {
    var result = ""
    result += genEdgeStruct(s)
    result += "typedef struct  { ";
    result += genStructMembers(inputs.map(p => p.name))
    result += "} " + getInputName(s) + " deriving(Bits, Eq);\n"
    result += "interface " + getInterfaceName(s) + ";\n"
    result += "  method Action " + s.name + "(" +
      getInputName(s) + " " + getEdgeParam() + ");\n"
    result += "endinterface\n"
    result
  }

  def genFirstStageModule(s: PStage, inputs: List[Param]): String = {
    var result = "module " + genModuleName(s) + "("
    result +=  s.succs.map(stg => getInterfaceName(stg) + " " +
      genParamName(stg) ).mkString(", ")
    result += ");\n\n"
    val inputFifo = "input_" + s.name
    result += "  " + getFifoType(getInputName(s)) + " " + inputFifo +
        " <- " + fifoModuleName + ";\n"
    result += "  method Action " + s.name + "(" +
      getInputName(s) + " " + getEdgeParam() + ");\n"
    result += "    " + inputFifo + ".enq(" + getEdgeParam() + ");\n"
    result += "  endmethod\n"
    result += genStageRules(s, "  ")
    result += "\nendmodule\n"
    result
  }

  def genStageModule(s: PStage): String = {
    var result = genModuleName(s) + "("
    result +=  s.succs.map(stg => getInterfaceName(stg) + " " +
      genParamName(stg) ).mkString(", ")
    result += ");\n\n"
    s.inEdges.foreach(e => {
      result += "  " + getFifoType(getEdgeName(e)) + " " + getInputParam(e) +
        " <- " + fifoModuleName + ";\n"
    })
    result += genStageRules(s, "  ")
    s.inEdges.foreach(e => {
      result += genRecieve(e, "  ")
    })
    result += "\nendmodule\n"
    result
  }

  def genStageRules(s: PStage, indent: String = ""): String = {
    var result = ""
    val (condIn, uncondIn) = s.inEdges.partition(e => e.condRecv.isDefined)
    //Do unconditional reads
    var doUncondReads = ""
    uncondIn.foreach(e => {
      val ein = getInputParam(e) + ".first"
      e.values.foreach(id => {
        result += indent + printBSVType(id.typ.get) + " " + id.v + " = " +
          ein + "." + id.v + ";\n"
      })
      doUncondReads += indent*2 + getInputParam(e) + ".deq();\n";
    })

    //Declare conditionally read variables as wires
    condIn.foldLeft(Set[Id]())((s, e) => {
      s ++ e.values
    }).foreach(id => {
      result += indent + "Wire #(" + printBSVType(id.typ.get) + ") " +
        id.v + " <- mkWire;\n"
    })

    //Do conditional Reads
    //TODO add rule condition which is !execute_blocked
    condIn.foreach(e => {
      result += indent + "rule read" + e.from.name +
        "(" + printBSVExpr(e.condRecv.get) + ");\n"
      result += doUncondReads
      val ein = getInputParam(e) + ".first"
      e.values.foreach(id => {
        result += indent*2 + id.v + " <= " + ein + "." + id.v + ";\n"
      })
      result += indent + "endrule;\n\n"
    })


    //Do Real Execution
    //TODO break into multiple rules for each set of conditional successors
    result += indent + "(* fire_when_enabled *)\n" + indent + "rule execute;\n"
    //print commands
    //TODO lift the combinational ones out to the module body
    s.cmds.foreach(c => {
      result += printBSVCommand(c, indent + "  ") + ";\n"
    })
    //send data out on output queues
    s.outEdges.foreach(e => {
      val condStr = e.condSend match {
        case Some(v) => "if (" + printBSVExpr(v, indent*2) + ") "
        case None => ""
      }
      val nstage = genParamName(e.to)
      val arg = getEdgeName(e) + "{" + e.values.map(id => {
        id.v + ":" + id.v
      }).mkString(",") + "}"
      result += indent*2 + condStr +
        nstage + "." + getSendName(e) + "(" + arg + ");\n"
    })
    result += indent + "endrule\n"
    result
  }

  def genRecieve(e: PipelineEdge, indent: String = ""): String = {
    var result = indent + "method Action " + getSendName(e) +
      "(" + getEdgeName(e) +  " " + getEdgeParam() +");\n"
    result += indent + indent + getInputParam(e) + ".enq(" + getEdgeParam() + ");\n"
    result += indent + "endmethod\n";
    result
  }

}
