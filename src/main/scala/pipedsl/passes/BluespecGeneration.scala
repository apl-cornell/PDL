package pipedsl.passes

import pipedsl.common.BSVPrettyPrinter._
import pipedsl.common.DAGSyntax._
import pipedsl.common.Errors.MissingType
import pipedsl.common.Syntax._
import pipedsl.common.Utilities._
import pprint.pprintln

object BluespecGeneration {

  val fifoType = "FIFOF"
  val fifoModuleName = "mkFIFOF"

  def getFifoType(t: String): String = {
    fifoType + " #(" + t +")"
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
    s.inEdges.foreach(e => {
      result += genRecieve(e, "  ")
    })
    result += genStageRules(s, "  ")
    result += "\nendmodule\n"
    result
  }

  def genStageRules(s: PStage, indent: String = ""): String = {
    var result = indent + "rule execute;\n"
    s.cmds.foreach(c => {
      result += printBSVCommand(c, indent + "  ") + "\n"
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
