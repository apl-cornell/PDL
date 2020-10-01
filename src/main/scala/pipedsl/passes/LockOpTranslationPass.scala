package pipedsl.passes

import pipedsl.common.DAGSyntax.PStage
import pipedsl.common.Dataflow._
import pipedsl.common.Errors.InvalidLockState
import pipedsl.common.Locks._
import pipedsl.common.Syntax._
import pipedsl.common.Utilities.flattenStageList
import pipedsl.passes.Passes.StagePass

object LockOpTranslationPass extends StagePass[List[PStage]] {


  private def lockVar(l: Id): EVar = {
    val res = EVar(Id("_lock_id_" + l.v))
    res.typ = Some(TRequestHandle(l, isLock = true))
    res.id.typ = res.typ
    res
  }

  override def run(stgs: List[PStage]): List[PStage] = {
    val (lockStarts, lockEnds) = worklist(flattenStageList(stgs), LockStateInfo)
    flattenStageList(stgs).foreach(s => {
      replaceLockOps(s, lockStarts(s.name), lockEnds(s.name))
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
  private def replaceLockOps(stg: PStage, startStates: Map[Id, LockState], endStates: Map[Id, LockState]): Unit = {
    val notlockCmds = stg.getCmds.filter { case _: CLockOp => false; case _ => true }
    //find the locks whose state changes in this stage:
    val changingLocks = endStates.keys.filter(m => { startStates.getOrElse(m, Free) != endStates(m) })
    val newlockcmds = changingLocks.foldLeft(List[Command]())((cmds, l) => {
      startStates.getOrElse(l, Free) match {
        case Free => endStates(l) match {
          case Free => cmds //nothing to do!
          case Reserved => cmds :+ IReserveLock(lockVar(l), l)
          case Acquired => cmds :+ ICheckLockFree(l) :+ IReserveLock(lockVar(l), l)
          case Released => cmds :+ ICheckLockFree(l) //don't actually acquire the lock, just check that we can
        }
        case Reserved => endStates(l) match {
          case Acquired => cmds :+ ICheckLockOwned(l, lockVar(l)) //just wait until our reservation is fulfilled
          case Released => cmds :+ ICheckLockOwned(l, lockVar(l)) :+ IReleaseLock(l, lockVar(l))
          case s@_ => throw InvalidLockState(l.pos, l.v, s, Acquired)
        }
        case Acquired => cmds :+ IReleaseLock(l, lockVar(l)) //only possible transition is release
        case Released => throw InvalidLockState(l.pos, l.v, Released, Acquired)
      }
    })
    stg.setCmds(notlockCmds ++ newlockcmds)
  }
}
