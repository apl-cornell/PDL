/* LockEliminationPass.scala */
package pipedsl.passes

import pipedsl.common.DAGSyntax.{IfStage, PStage}
import pipedsl.common.Locks.eliminateLockRegions
import pipedsl.common.Syntax.{Command, ICondCommand, ILockNoOp}
import pipedsl.common.Utilities.flattenStageList
import pipedsl.passes.Passes.StagePass

object LockEliminationPass extends StagePass[List[PStage]] {

  override def run(stgs: List[PStage]): List[PStage] = {
    flattenStageList(stgs).foreach { s =>
      eliminateLockRegions(s)
      removeNops(s)
    }
    stgs
  }


  //Lock No-Ops were introduced by prior passes to track sets of
  //possible lock states. They're no longer needed at this point.
  private def removeNops(stg: PStage): Unit = {
    stg.setCmds(stg.getCmds.foldLeft(List[Command]())((l, c) => c match {
      case ICondCommand(cond, cs) =>
        l :+ ICondCommand(cond, cs.filterNot {
        case _: ILockNoOp => true
        case _ => false
      })
      case ILockNoOp(_) => l
      case _ => l :+ c
    }))
  }

}
