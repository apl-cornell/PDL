package pipedsl.passes

import pipedsl.common.DAGSyntax.PStage
import pipedsl.common.Errors.UnexpectedCommand
import pipedsl.common.Locks._
import pipedsl.common.Syntax._
import pipedsl.common.Utilities.{flattenStageList, updateListMap}
import pipedsl.passes.Passes.StagePass

object LockOpTranslationPass extends StagePass[List[PStage]] {


  private def lockVar(l: Id): EVar = {
    val res = EVar(Id("_lock_id_" + l.v))
    res.typ = Some(TRequestHandle(l, isLock = true))
    res.id.typ = res.typ
    res
  }

  override def run(stgs: List[PStage]): List[PStage] = {
    flattenStageList(stgs).foreach(s => {
      eliminateLockRegions(s)
      translateLockOps(s)
    })
    stgs
  }

  /**
   * Replace the existing lock ops where necessary.
   * Specifically, add in "lock check" commands that block
   * until the lock is acquirable and remove the "acquire"
   * commands that are unnecessary thanks to the lock already being "reserved"
   * @param stg - The stage to modify
   */
  private def translateLockOps(stg: PStage): Unit = {
    val (lockCmds, notlockCmds) = stg.getCmds.partition { case _: CLockOp => true; case _ => false }
    val lockmap = lockCmds.foldLeft(Map[LockArg, List[Command]]())((m, lc) => lc match {
      case c@CLockOp(mem, _) => updateListMap(m, mem, translateOp(c))
      case _ => throw UnexpectedCommand(lc)
    })
    val newlockcmds = lockmap.keys.foldLeft(List[Command]())((cs, lid) => {
      cs ++ mergeLockOps(lid.id, lockmap(lid))
    })
    stg.setCmds(notlockCmds ++ newlockcmds)
  }

  private def translateOp(c: CLockOp): Command = c.op match {
    case Free => ICheckLockFree(c.mem.id)
    case Reserved => IReserveLock(lockVar(c.mem.id), c.mem.id)
    case Acquired => ICheckLockOwned(c.mem.id, lockVar(c.mem.id))
    case Released => IReleaseLock(c.mem.id, lockVar(c.mem.id))
  }

}
