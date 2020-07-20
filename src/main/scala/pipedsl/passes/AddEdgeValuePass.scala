package pipedsl.passes

import pipedsl.common.Dataflow._
import pipedsl.common.DAGSyntax.{IfStage, PStage, PipelineEdge, SpecStage}
import pipedsl.common.Syntax._
import pipedsl.common.Utilities._
import pipedsl.passes.Passes.StagePass

object AddEdgeValuePass extends StagePass[List[PStage]] {

  override def run(stgs: List[PStage]): List[PStage] = {
    val (usedIns, _) = worklist(flattenStageList(stgs), UsedInLaterStages)
    stgs.foreach(s => addEdgeValues(s, usedIns))
    stgs
  }

  def addEdgeValues(stg: PStage, usedIns: DFMap[Id]): Unit = {
    stg match {
      case s: IfStage => {
        val trueEdge = PipelineEdge(Some(s.cond), s, s.trueStages.head, usedIns(s.trueStages.head.name))
        val notCond = EUop(NotOp(), s.cond)
        val falseEdge = PipelineEdge(Some(notCond), s, s.falseStages.head, usedIns(s.falseStages.head.name))
        val choiceEdge = PipelineEdge(None, s, s.joinStage, Set(s.condVar.id))
        stg.setEdges(stg.inEdges + trueEdge + falseEdge + choiceEdge)
        s.trueStages.foreach(st => addEdgeValues(st, usedIns))
        s.falseStages.foreach(sf => addEdgeValues(sf, usedIns))
      }
      case s: SpecStage => {
        //Split input of join stage into two sets of inputs to be expected
        //current split algo is send everything from verify, except outputs only produced by spec

        val producedBySpec = usedIns(s.joinStage.name) -- usedIns(s.specStages.head.name)
        val usedSpecWorklist = Analysis(isForward = false, producedBySpec, mergeUsedVars, transferUsedVars)
        val (specUsedIns, _) = worklist(flattenStageList(s.specStages), usedSpecWorklist)
        val specUsedInsJoin = specUsedIns + (s.joinStage.name -> producedBySpec)
        //add edge values in the context where the join only expects certain values
        s.specStages.foreach(st => addEdgeValues(st, specUsedInsJoin))

        val producedByVerif = usedIns(s.joinStage.name) -- producedBySpec
        val usedVerifWorklist = Analysis(isForward = false, producedByVerif, mergeUsedVars, transferUsedVars)
        val (verifUsedIns, _) = worklist(flattenStageList(s.verifyStages), usedVerifWorklist)
        val verifUsedInsJoin = verifUsedIns + (s.joinStage.name -> producedByVerif)
        s.verifyStages.foreach(st => addEdgeValues(st, verifUsedInsJoin))

        //add edges to beginning of spec section
        val specEdge = PipelineEdge(None, s, s.specStages.head, specUsedIns(s.specStages.head.name))
        val verifEdge = PipelineEdge(None, s, s.verifyStages.head, verifUsedIns(s.verifyStages.head.name))
        s.setEdges(s.inEdges + specEdge + verifEdge)
      }
      case _ => {
        stg.setEdges(stg.outEdges.map(edge => {
          PipelineEdge(edge.cond, edge.from, edge.to, usedIns(edge.to.name))
        }) ++ stg.inEdges)
      }
    }
  }
}
