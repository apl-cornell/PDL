package pipedsl.passes

import pipedsl.common.BSVPrettyPrinter._
import pipedsl.common.DAGSyntax._
import pipedsl.common.Errors.MissingType
import pipedsl.common.Syntax._
import pipedsl.common.Utilities._
import pprint.pprintln

object BluespecGeneration {

  def run(firstStage: PStage, inputs: List[Param], rest: List[PStage]): String = {
    pprintln(genFirstStageDefs(firstStage, inputs))
    rest.foreach(stg => {
      pprintln(genEdgeStruct(stg))
    })
    rest.foreach(stg => {
      pprintln(genStageInterface(stg))
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

  def genStageInterface(s: PStage): String = {
    var result = "interface S_" + s.name + ";\n"
    s.inEdges.foreach(e => {
      result += "  method Action " + e.from.name + "To" + e.to.name + "(" + getEdgeName(e) + ");\n"
    })
    result += "endinterface\n"
    result
  }

  def genFirstStageDefs(s: PStage, inputs: List[Param]): String = {
    var result = ""
    result += genEdgeStruct(s)
    result += "typedef struct  { ";
    result += genStructMembers(inputs.map(p => p.name))
    result += "} " + getInputName(s) + " deriving(Bits, Eq);\n"
    result += "interface S_" + s.name + ";\n"
    result += "  method Action " + s.name + "(" + getInputName(s) + ");\n"
    result += "endinterface\n"
    result
  }

  def genStageRules(s: PStage): String = {

    ""
  }

}
