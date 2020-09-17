package pipedsl.passes

import pipedsl.common.DAGSyntax._
import pipedsl.common.Syntax._
import pipedsl.common.Utilities.{andExpr, getReachableStages}
import pipedsl.passes.Passes.StagePass

/**
 * The SplitStages pass forces IF statements to create
 * a new stage for each of their branches. In the event
 * that one or both of those branches are combinational,
 * we can actually merge those stages together.
 *
 * Furthermore, all IF stages can be combined with their
 * predecessor, since the SplitStages pass also introduces
 * new stages whenever there is a conditional branch.
 *
 */
object CollapseStagesPass extends StagePass[List[PStage]] {

  override def run(stgs: List[PStage]): List[PStage] = {
    stgs.foreach(s => simplifyIfs(s))
    //only return the stgs reachable from the start node
    //since we've cut off some nodes from the graph
    getReachableStages(stgs.head)
  }

  private def simplifyIfs(stg: PStage): Unit = stg match {
    case s: IfStage =>
      //TODO recursively merge inner portions of the pipeline
      s.trueStages.foreach(t => simplifyIfs(t))
      s.falseStages.foreach(f => simplifyIfs(f))
      //Check if the branches are combinational
      val isTrueComb = s.trueStages.head.outEdges.exists(e => e.to == s.joinStage)
      val isFalseComb = s.falseStages.head.outEdges.exists(e => e.to == s.joinStage)
      //Merge in the first true and false stages since that delay is artificial
      mergeStages(s, s.trueStages.head)
      mergeStages(s, s.falseStages.head)
      //Update IF stage metadata
      s.trueStages = s.trueStages.tail
      s.falseStages = s.falseStages.tail
      //Get the inputs to the join stage from any conditional edge
      val joinStgInputs = s.joinStage.inEdges.filter(e => e.condRecv.isDefined).head.values
      //Special case here, where we can merge all of the output edges into one
      if (isTrueComb && isFalseComb) {
        //join stage can be eliminated completely in this case
        val newOutEdge = PipelineEdge(None, None, s, s.joinStage, joinStgInputs)
        s.removeEdgesTo(s.joinStage)
        s.addEdge(newOutEdge)
        mergeStages(s, s.joinStage)
      }
      //there must only be one by construction
      val priorstg = s.inEdges.head.from
      //merge this into the prior stage since that delay was added unnecessarily
      mergeStages(priorstg, s)
    case stage: SpecStage =>
    case _ =>
  }

  private def addCondStmts(stg: PStage, cond: Expr, stmts: Iterable[Command]): Unit = {
    val condstmts = stmts.map(s => ICondCommand(cond, s))
    stg.cmds = stg.cmds ++ condstmts
  }

  /**
   * Merge src into target, but execute it conditionally
   * @param target
   * @param src
   */
  private def mergeStages(target: PStage, src: PStage) = {
    if (src.inEdges.exists(e => e.from != target) ||
      !src.inEdges.exists(e => e.from == target)) {
      throw new RuntimeException(s"Cannot merge ${src.name.v} since it has inputs other than ${target.name.v}")
    }
    val existingedges = target.outEdges.filter(e => e.to == src)
    if (existingedges.size > 1) {
      throw new RuntimeException(s"Cannot merge ${src.name.v} into ${target.name.v} since they have multiple edges")
    }
    val cond = existingedges.head.condSend
    //merge in the commands
    if (cond.isDefined) {
      addCondStmts(target, cond.get, src.cmds)
    } else {
      target.cmds = target.cmds ++ src.cmds
    }
    //merge in the output edge from src with target as the new from stage
    //use the old condsend to src as the new condsend
    target.removeEdgesTo(src)
    val outedges = src.outEdges
    val outdests = outedges.foldLeft(Set[PStage]())((s, e) => s + e.to)
    outdests.foreach(dest => src.removeEdgesTo(dest))
    outedges.foreach(e => {
      val newedge = PipelineEdge(andExpr(cond, e.condSend), e.condRecv, target, e.to, e.values)
      target.addEdge(newedge)
    })
  }
}
