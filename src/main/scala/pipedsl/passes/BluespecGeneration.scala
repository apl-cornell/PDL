package pipedsl.passes

import pipedsl.common.BSVPrettyPrinter._
import pipedsl.common.DAGSyntax._
import pipedsl.common.Errors.MissingType
import pipedsl.common.Utilities._
import pipedsl.passes.Passes.StagePass
import pprint.pprintln

object BluespecGeneration extends StagePass[String] {

  override def run(s: List[PStage]): String = {
    s.foreach(stg => {
      pprintln(genEdgeStruct(stg))
    })
    ""
  }

  def getEdgeName(e: PipelineEdge): String = {
    "E_" + e.from.name + "-" + e.to.name;
  }

  def genEdgeStruct(s: PStage): String = {
    s.outEdges.foldLeft("")((str, e) => {
      var result = "typedef struct { ";
      e.values.foreach(id => {
        result += printBSVType(id.typ.getOrThrow(MissingType(id.pos, id.v))) +
          " " + id.v + "; "
      })
      result += "} " + getEdgeName(e) + " deriving(Bits, Eq);\n"
      str + result
    })
  }

  def genStageInterface(s: PStage): String = {
    var result = "interface Stage_" + s.name + ";\n"

    result
  }
}
