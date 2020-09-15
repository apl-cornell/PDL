package pipedsl.passes

import pipedsl.common.DAGSyntax._
import pipedsl.common.Syntax._
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
    stgs.map(s => eliminateIfs(s))
  }

  private def eliminateIfs(stg: PStage): PStage = stg match {
    case s: IfStage =>
      //TODO recursively merge inner portions of the pipeline
      //Check if the branches are combinational
      val isTrueComb = s.trueStages.head == s.trueStages.last
      val isFalseComb = s.falseStages.head == s.falseStages.last
      //Merge in the first true and false stages since that delay is artificial
      mergeStages(s, s.trueStages.head)
      mergeStages(s, s.falseStages.head)
      //Update IF stage metadata
      //TODO convert this to a normal PStage so we can eliminate this extra bookkeeping
      s.trueStages = s.trueStages.tail
      s.falseStages = s.falseStages.tail
      //Get the inputs to the join stage from any conditional edge
      val joinStgInputs = s.joinStage.inEdges.filter(e => e.condRecv.isDefined).head.values
      //Special case here, where we can merge all of the output edges into one
      if (isTrueComb && isFalseComb) {
        val newOutEdge = PipelineEdge(None, None, s, s.joinStage, joinStgInputs)
        s.removeEdgesTo(s.joinStage)
        s.addEdge(newOutEdge)
      }
      s
    case stage: SpecStage =>
      stage
    case _ => stg
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
      val newedge = PipelineEdge(cond, e.condRecv, target, e.to, e.values)
      target.addEdge(newedge)
    })

  }
}
