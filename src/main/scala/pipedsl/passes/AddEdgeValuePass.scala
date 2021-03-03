package pipedsl.passes

import pipedsl.common.Dataflow._
import pipedsl.common.DAGSyntax.{IfStage, PStage, PipelineEdge, SpecStage, addValues}
import pipedsl.common.Syntax._
import pipedsl.common.Utilities._
import pipedsl.passes.Passes.StagePass

/**
 * This uses a live variable dataflow analysis to
 * determine which values must be communicated from stage to stage.
 * It also add special control values for special stages (like IF stages)
 * that are used to conditionally activate later stages.
 */
object AddEdgeValuePass extends StagePass[List[PStage]] {

  override def run(stgs: List[PStage]): List[PStage] = {
    val (usedIns, _) = worklist(flattenStageList(stgs), UsedInLaterStages)
    val (_, canSendOut) = worklist(flattenStageList(stgs), CanSendToLaterStages)
    stgs.foreach(s => addEdgeValues(s, usedIns, canSendOut, Set[Id]()))
    stgs
  }

  /**
   * For each outgoing edge in the stage, using the live variable analysis,
   * add values which dictate which variables should be communicated along that
   * edge. Only condition variables (the conditions of if statements) can possible travel
   * along multiple paths to the same node. We treat IFStages differently to manually
   * propagate the condition variable along its own edge and forbid the branches from
   * tranmitting that variable.
   *
   * This should *not* be called with a flattened stage list since it recursively calls
   * children stages for special types of stages.
   *
   * @param stg The stage whose outgoing edges we will modify
   * @param usedIns The result of a live variable analysis
   * @param canSend The result of a defined variable analysis - this is used to
   *                avoid adding "false dependencies" to edges (which result because we do not use phi nodes)
   * @param dontSends The set of variables which we must not add to edges even if the analysis found them live
   *                  because they are being sent elsewhere.
   */
  private def addEdgeValues(stg: PStage, usedIns: DFMap[Set[Id]], canSend: DFMap[Set[Id]], dontSends: Set[Id]): Unit = {
    stg match {
      case s: IfStage =>
        val choiceEdge = PipelineEdge(None, None, s, s.joinStage, Set(s.condVar.id))
        val newOutEdges = s.outEdges.map(e => {
          addValues(usedIns(e.to.name).diff(dontSends).intersect(canSend(s.name)), e)
        })
        s.setEdges(s.inEdges ++ newOutEdges + choiceEdge)
        s.condStages.foreach(sc => sc.foreach(stg => addEdgeValues(stg, usedIns, canSend, dontSends + s.condVar.id)))
        s.defaultStages.foreach(sf => addEdgeValues(sf, usedIns, canSend, dontSends + s.condVar.id))
      case s: SpecStage =>
        //Split input of join stage into two sets of inputs to be expected
        //current split algo is send everything from verify, except outputs only produced by spec
        //TODO this is all made up
        val producedBySpec = usedIns(s.joinStage.name) -- usedIns(s.specStages.head.name)
        val usedSpecWorklist = Analysis(isForward = false, producedBySpec, mergeUsedVars, transferUsedVars)
        val (specUsedIns, _) = worklist(flattenStageList(s.specStages), usedSpecWorklist)
        val specUsedInsJoin = specUsedIns + (s.joinStage.name -> producedBySpec)
        //add edge values in the context where the join only expects certain values
        s.specStages.foreach(st => addEdgeValues(st, specUsedInsJoin, canSend, dontSends))

        val producedByVerif = usedIns(s.joinStage.name) -- producedBySpec
        val usedVerifWorklist = Analysis(isForward = false, producedByVerif, mergeUsedVars, transferUsedVars)
        val (verifUsedIns, _) = worklist(flattenStageList(s.verifyStages), usedVerifWorklist)
        val verifUsedInsJoin = verifUsedIns + (s.joinStage.name -> producedByVerif)
        s.verifyStages.foreach(st => addEdgeValues(st, verifUsedInsJoin, canSend, dontSends))

        //add edges to beginning of spec section
        val specEdge = PipelineEdge(None, None, s, s.specStages.head, specUsedIns(s.specStages.head.name))
        val verifEdge = PipelineEdge(None, None,  s, s.verifyStages.head, verifUsedIns(s.verifyStages.head.name))
        s.setEdges(s.inEdges + specEdge + verifEdge)
      case _ =>
        stg.setEdges(stg.outEdges.map(edge => {
          //send everything except for the dontSends
          val tosend = usedIns(edge.to.name).diff(dontSends).intersect(canSend(stg.name))
          PipelineEdge(edge.condSend, edge.condRecv, edge.from, edge.to, tosend)
        }) ++ stg.inEdges)
    }
  }
}
