package pipedsl.passes

import pipedsl.common.DAGSyntax.PStage
import pipedsl.common.Syntax
import pipedsl.common.Syntax.{Command, ICondCommand, ILockNoOp}
import pipedsl.passes.Passes.StagePass

object LockEliminationPass extends StagePass[List[PStage]] {

  override def run(stgs: List[PStage]): List[PStage] = {
    stgs.foreach(s => {
      //yes this is supposed to run in both passes
      LockOpTranslationPass.eliminateLockRegions(s)
      removeNops(s)
    })
    stgs
  }

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
