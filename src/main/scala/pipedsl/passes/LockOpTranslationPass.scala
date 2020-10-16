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
      translateLockOps(s, lockStarts(s.name), lockEnds(s.name))
      eliminateLockRegions(s)
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
  private def translateLockOps(stg: PStage, startStates: Map[Id, LockState], endStates: Map[Id, LockState]): Unit = {
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
}
