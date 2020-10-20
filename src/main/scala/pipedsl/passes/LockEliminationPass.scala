package pipedsl.passes

import pipedsl.common.DAGSyntax.PStage
import pipedsl.common.Syntax.{CLockEnd, CLockStart, Command, ICondCommand, ILockNoOp, Id}
import pipedsl.passes.Passes.StagePass

object LockEliminationPass extends StagePass[List[PStage]] {

  override def run(stgs: List[PStage]): List[PStage] = {
    stgs.foreach(s => {
      //yes this is supposed to run in both passes
      eliminateLockRegions(s)
      removeNops(s)
    })
    stgs
  }

  private def eliminateLockRegions(stg: PStage): Unit = {
    //get all ids that we start or stop regions for in this stage
    val (startedRegions, endedRegions) = stg.getCmds.foldLeft(
      (Set[Id](), Set[Id]()))((s:(Set[Id], Set[Id]), c) => c match {
      case CLockStart(mod) => (s._1 + mod, s._2)
      case CLockEnd(mod) => (s._1, s._2 + mod)
      case _ => s
    })
    //anytime we start and end in the same stage, we don't need those
    val unnecessaryReservations = startedRegions.intersect(endedRegions)
    //returns only necessary reservation cmds and all other cmds
    val newCmds = stg.getCmds.filter {
      case CLockStart(mod) if unnecessaryReservations.contains(mod) => false
      case CLockEnd(mod) if unnecessaryReservations.contains(mod) => false
      case _ => true
    }
    stg.setCmds(newCmds)
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
