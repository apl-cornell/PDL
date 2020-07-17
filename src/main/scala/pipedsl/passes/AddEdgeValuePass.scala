package pipedsl.passes

import pipedsl.common.Dataflow._
import pipedsl.common.DAGSyntax.{IfStage, PStage, PipelineEdge}
import pipedsl.common.Syntax._
import pipedsl.common.Utilities._
import pipedsl.passes.Passes.StagePass

object AddEdgeValuePass extends StagePass[List[PStage]] {

  override def run(stgs: List[PStage]): List[PStage] = {
    val (usedIns, usedOuts) = worklist(flattenStageList(stgs), UsedInLaterStages)
    stgs.foreach(s => addEdgeValues(s, usedIns))
    stgs
  }

  def addEdgeValues(stg: PStage, usedIns: DFMap[Id]): Unit = {
    stg.edges = stg.outEdges.map(edge => {
      PipelineEdge(edge.cond, edge.from, edge.to, usedIns(edge.to.name))
    }) ++ stg.inEdges
    stg match {
      case s: IfStage => {
        val trueEdge = PipelineEdge(Some(s.cond), s, s.trueStages.head, usedIns(s.trueStages.head.name))
        val notCond = EUop(NotOp(), s.cond)
        val falseEdge = PipelineEdge(Some(notCond), s, s.falseStages.head, usedIns(s.falseStages.head.name))
        val choiceEdge = PipelineEdge(None, s, s.joinStage, Set(s.condId))
        stg.edges = stg.inEdges + trueEdge + falseEdge + choiceEdge
        s.trueStages.foreach(st => addEdgeValues(st, usedIns))
        s.falseStages.foreach(sf => addEdgeValues(sf, usedIns))
      }
      case _ => stg.succs.foreach(s => addEdgeValues(s, usedIns))
    }
  }
}
