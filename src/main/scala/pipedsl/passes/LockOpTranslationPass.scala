package pipedsl.passes

import pipedsl.common.DAGSyntax.PStage
import pipedsl.common.Dataflow._
import pipedsl.common.Locks.LockState
import pipedsl.common.Syntax._
import pipedsl.common.Utilities.flattenStageList
import pipedsl.passes.Passes.StagePass

object LockOpTranslationPass extends StagePass[List[PStage]] {

  override def run(stgs: List[PStage]): List[PStage] = {
    val lockInfo = worklist(flattenStageList(stgs), LockStateInfo)
    stgs
  }

  /**
   * If a stage tries to acquire and release a lock,
   * then replace both ops with a "canAcquire" op
   * which just checks that the lock is available.
   *
   * This only applies when the stage starts with
   * the lock in the Free state. If the lock was reserved,
   * then we need to replace the "acquire" with a "canacquire"
   * but leave the "release" untouched.
   * @param stg
   */
  private def replaceLockOps(stg: PStage): Unit = {
    val (locks, notlocks) = stg.cmds.partition { case _: CLockOp => true; case _ => false }
    val lockops: List[CLockOp] = notlocks.foldLeft(List[CLockOp]())((l, c) => c match {
      case c: CLockOp => l :+ c
      case _ => l
    })
    if (lockops.contains(LockState.Acquired) && lockops.contains(LockState.Released)) {

    }
  }
}
