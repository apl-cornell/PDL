package pipedsl.passes

import pipedsl.common.DAGSyntax.PStage
import pipedsl.common.Errors.UnexpectedCommand
import pipedsl.common.Locks._
import pipedsl.common.Syntax._
import pipedsl.common.Utilities.{flattenStageList, updateListMap}
import pipedsl.passes.Passes.StagePass

object LockOpTranslationPass extends StagePass[List[PStage]] {


  private def lockVar(l: LockArg): EVar = {
    val lockname = "_lock_id_" + l.id.v + (if (l.evar.isDefined) "_" + l.evar.get.id.v else "")
    val res = EVar(Id(lockname))
    res.typ = Some(TRequestHandle(l.id, isLock = true))
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
      case c@CLockOp(mem, _, _) => updateListMap(m, mem, translateOp(c))
      case _ => throw UnexpectedCommand(lc)
    })
    val newlockcmds = lockmap.keys.foldLeft(List[Command]())((cs, lid) => {
      cs ++ mergeLockOps(lid, lockmap(lid))
    })
    stg.setCmds(notlockCmds ++ newlockcmds)
  }

  private def translateOp(c: CLockOp): Command = {
    //TODO make this cleaner lol
    c.op match {
      case Free => {
        val i = ICheckLockFree(c.mem)
        i.memOpType = c.memOpType
        i.granularity = c.granularity
        i
      }
      case Reserved => {
        val i = IReserveLock(lockVar(c.mem), c.mem)
        i.memOpType = c.memOpType
        i.granularity = c.granularity
        i
      }
      case Acquired => {
        val i = ICheckLockOwned(c.mem, lockVar(c.mem))
        i.memOpType = c.memOpType
        i.granularity = c.granularity
        i
      }
      case Released => {
        val i = IReleaseLock(c.mem, lockVar(c.mem))
        i.memOpType = c.memOpType
        i.granularity = c.granularity
        i
      }
    }

  }

}
