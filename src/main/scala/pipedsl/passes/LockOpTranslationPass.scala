package pipedsl.passes

import pipedsl.common.DAGSyntax.PStage
import pipedsl.common.Dataflow._
import pipedsl.common.Errors.InvalidLockState
import pipedsl.common.Locks.LockState._
import pipedsl.common.Syntax._
import pipedsl.common.Utilities.flattenStageList
import pipedsl.passes.Passes.StagePass

object LockOpTranslationPass extends StagePass[List[PStage]] {

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
    val notlockCmds = stg.cmds.filter { case _: CLockOp => false; case _ => true }
    //find the locks whose state changes in this stage:
    val changingLocks = startStates.keys.filter(m => { startStates(m) != endStates(m) })
    val newlockcmds = changingLocks.foldLeft(List[Command]())((cmds, l) => {
      startStates(l) match {
        case Free => endStates(l) match {
          case Reserved => cmds :+ CLockOp(l, Reserved)
          case Acquired => cmds :+ ICheckLock(l) :+ CLockOp(l, Acquired)
          case Released => cmds :+ ICheckLock(l) //don't actually acquire the lock, just check that we can
        }
        case Reserved => endStates(l) match {
          case Acquired => cmds :+ ICheckLock(l) //just wait until our reservation is fulfilled
          case Released => cmds :+ ICheckLock(l) :+ CLockOp(l, Released)
          case s@_ => throw InvalidLockState(l.pos, l.v, s, Acquired)
        }
        case Acquired => cmds :+ CLockOp(l, Released) //only possible transition is release
        case Released => throw InvalidLockState(l.pos, l.v, Released, Acquired)
      }
    })
    stg.cmds = newlockcmds ++ notlockCmds
  }
}
