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
    stgs.map(s => mergeStage(s))
  }

  private def mergeStage(stg: PStage): PStage = stg match {
    case s: IfStage =>
      //TODO recursively merge inner portions of the pipeline
      val isTrueComb = s.trueStages.head == s.trueStages.last
      val isFalseComb = s.falseStages.head == s.falseStages.last
      val joinStgInputs = s.joinStage.inEdges.filter(e => e.condRecv.isDefined).head.values
      (isTrueComb, isFalseComb) match {
        case (false, false) => ()
        case (false, true) => {
          addCondStmts(s, s.notCond, s.falseStages.head.cmds)
          //conditionally send and receive on this edge directly from the IF beginning to the Join stage
          //replacing the old edges to the FALSE branch and join stage
          val newOutEdge = PipelineEdge(Some(s.notCond), Some(s.notCond), s, s.joinStage, joinStgInputs)
          s.removeEdgesTo(s.falseStages.head)
          s.addEdge(newOutEdge)
          s.falseStages.head.removeEdgesTo(s.joinStage)
          s.falseStages = List()
        }
        case (true, false) => {
          addCondStmts(s, s.condVar, s.trueStages.head.cmds)
          //conditionally send and receive on this edge directly from the IF beginning to the Join stage
          //replacing the old edges to the TRUE branch and join stage
          val newOutEdge = PipelineEdge(Some(s.condVar), Some(s.condVar), s, s.joinStage,joinStgInputs)
          s.removeEdgesTo(s.trueStages.head)
          s.addEdge(newOutEdge)
          s.trueStages.head.removeEdgesTo(s.joinStage)
          s.trueStages = List()
        }
        case (true, true) => {
          addCondStmts(s, s.condVar, s.trueStages.head.cmds)
          addCondStmts(s, s.notCond, s.falseStages.head.cmds)
          val newOutEdge = PipelineEdge(None, None, s, s.joinStage, joinStgInputs)
          s.removeEdgesTo(s.joinStage)
          s.removeEdgesTo(s.trueStages.head)
          s.removeEdgesTo(s.falseStages.head)
          s.falseStages.head.removeEdgesTo(s.joinStage)
          s.trueStages.head.removeEdgesTo(s.joinStage)
          s.trueStages = List()
          s.falseStages = List()
          s.addEdge(newOutEdge)
        }
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
}
