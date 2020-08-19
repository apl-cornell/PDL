package pipedsl.passes

import pipedsl.common.BSVPrettyPrinter._
import pipedsl.common.BSVSyntax._
import pipedsl.common.DAGSyntax._
import pipedsl.common.Errors.MissingType
import pipedsl.common.Syntax._
import pipedsl.common.Utilities._
import pprint.pprintln

object BluespecGeneration {

  val fifoType = "FIFOF"
  val fifoModuleName = "mkFIFOF"
  val firstStageIntName = "S_start"
  val firstStageStructName = "E_start"
  val firstStageName = "start"

  type EdgeTypes = Map[PipelineEdge, BStructDef]
  type StageTypes = Map[PStage, BInterfaceDef]

  def getFifoType(t: String): String = {
    fifoType + " #(" + t +")"
  }

  def getBSV(firstStage: PStage, inputs: List[Id], rest: List[PStage]): Option[BProgram] = {
    val edgeStructMap = getEdgeStructMap(rest)
    val firstStageStruct = getFirstStageStruct(inputs)
    val firstStageInterface = getFirstStageInterface(firstStageStruct.typ)
    val stageInterfacemap = getStageIntMap(rest, edgeStructMap)

    val structDefs = firstStageStruct +: edgeStructMap.values.toList
    val intDefs = firstStageInterface +: stageInterfacemap.values.toList
    None
  }

  def getFirstStageStruct(inputs: List[Id]): BStructDef = {
    val styp = BStruct(firstStageStructName, getBsvStructFields(inputs))
    BStructDef(styp, List("Bits", "Eq"))
  }

  def getFirstStageInterface(struct: BSVType): BInterfaceDef = {
    BInterfaceDef(firstStageIntName,
      List(BMethodSig(firstStageName, MethodType.Action, List(BParam("d_in",struct)))))
  }


  def getEdgeStructMap(stgs: List[PStage]): Map[PipelineEdge, BStructDef] = {
    stgs.foldLeft[Map[PipelineEdge, BStructDef]](Map())((m, s) => {
      s.inEdges.foldLeft[Map[PipelineEdge, BStructDef]](m)((ms, e) => {
        ms + (e -> getEdgeStruct(e))
      })
    })
  }

  def getEdgeStruct(e: PipelineEdge): BStructDef = {
    val styp = BStruct(firstStageStructName, getBsvStructFields(e.values))
    BStructDef(styp, List("Bits", "Eq"))
  }

  def getBsvStructFields(inputs: Iterable[Id]): List[BParam] = {
    inputs.foldLeft(List[BParam]())((l, id) => {
      l :+ BParam(id.v, toBSVType(id.typ.get))
    })
  }

  def getStageIntMap(stgs: List[PStage], edgeTypes: EdgeTypes): StageTypes = {
    stgs.foldLeft[StageTypes](Map())((m, s) => {
      m + (s -> getStageInterface(s, edgeTypes))
    })
  }

  def getStageInterface(stg: PStage, edgeTypes: EdgeTypes): BInterfaceDef = {
    val methodSigs = stg.inEdges.foldLeft[List[BMethodSig]](List())((l, e) => {
      l :+ BMethodSig(getSendName(e), MethodType.Action, List(BParam("d_in", edgeTypes(e).typ)))
    })
    BInterfaceDef("S_" + stg.name.v, methodSigs)
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

  def getParamName(s: PStage): String = {
    "s_" + s.name
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

  def getModuleName(s: PStage): String = {
    "module mk" + s.name
  }

  def genFirstStageModule(s: PStage, inputs: List[Param]): String = {
    var result = getModuleName(s) + "("
    result +=  s.succs.map(stg => getInterfaceName(stg) + " " +
      getParamName(stg) ).mkString(", ")
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
    var result = getModuleName(s) + "("
    result +=  s.succs.map(stg => getInterfaceName(stg) + " " +
      getParamName(stg) ).mkString(", ")
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
      val nstage = getParamName(e.to)
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
