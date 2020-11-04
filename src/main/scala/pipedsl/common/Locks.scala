package pipedsl.common

import pipedsl.common.DAGSyntax.PStage
import pipedsl.common.Dataflow.DFMap
import pipedsl.common.Errors.InvalidLockState
import pipedsl.common.Syntax.{CLockEnd, CLockOp, CLockStart, Command, EVar, ICheckLockFree, ICheckLockOwned, IReleaseLock, IReserveLock, Id, LockArg}
import pipedsl.common.Utilities.updateSetMap

import scala.util.parsing.input.Position

object Locks {

  sealed abstract class LockState(val name: String, val order: Int) extends Ordered[LockState] {
    override def compare(that: LockState): Int = this.order compare that.order
  }

  def join(l1: LockState, l2: LockState): LockState = {
    if (l1 < l2) {
      l2
    } else {
      l1
    }
  }

  def join(l1: Option[LockState], l2: Option[LockState]): LockState = (l1, l2) match {
    case (Some(ll), Some(rr)) => join(ll, rr)
    case (Some(ll), None) => ll
    case (None, Some(rr)) => rr
    case (None, None) => throw new RuntimeException("Shouldn't pass two Nones to join")
  }


  sealed trait LockType
  case object Specific extends LockType
  case object General extends LockType
  
  case object Free extends LockState("free", 0)
  case object Reserved extends LockState("reserved", 1)
  case object Acquired extends LockState("acquired", 2)
  case object Released extends LockState("released", 3)


  type LockHandleInfo = (EVar, EVar) //left = address variable, right = handle variable
  /**
   * After executing the current 'basic block' returns which locked
   * addresses may be reserved.
   * @param node
   * @param instates
   * @return
   */
  def transferMaybeReserved(node: PStage, instates: Map[Id, Set[LockHandleInfo]]): Map[Id, Set[LockHandleInfo]] = {
    var newMap = instates
    node.getCmds.foreach {
      case IReserveLock(handle, larg) if larg.evar.isDefined =>
        newMap = updateSetMap(newMap, larg.id, (larg.evar.get, handle))
      case IReleaseLock(larg, handle) if larg.evar.isDefined =>
        newMap.get(larg.id) match {
          case Some(s) => newMap = newMap.updated(larg.id, s.filter(info => info._1 != larg.evar.get))
          case None => ()
        }
      case _ => ()
    }
    newMap
  }
  def mergeMaybeReserved(node: PStage, instates: DFMap[Map[Id, Set[LockHandleInfo]]]): Map[Id, Set[LockHandleInfo]] = {
    instates.keys.foldLeft(Map[Id, Set[LockHandleInfo]]())((res, sid) => {
      val stgReserved = instates(sid)
      (res.keySet ++ stgReserved.keySet).map(k => {
        k-> ((res.get(k), stgReserved.get(k)) match {
          case (Some(s1), Some(s2)) => s1 ++ s2
          case (Some(s1), None) => s1
          case (None, Some(s2)) => s2
          case (None, None) => throw new RuntimeException("unreachable")
        })
      }).toMap
    })
  }

  def transferLockStates(node: PStage, instates: Map[LockArg, LockState]): Map[LockArg, LockState] = {
    var newMap = instates
    node.getCmds.foreach {
      case CLockOp(mem, op) => newMap = newMap.updated(mem, op)
      case _ => ()
    }
    newMap
  }

  //Need the node argument to fit the Dataflow signature
  def mergeLockStates(node: PStage, instates: DFMap[Map[LockArg, LockState]]): Map[LockArg, LockState] = {
    instates.keys.foldLeft(Map[LockArg, LockState]())((m, stg) => {
      val stgstates = instates(stg)
      (m.keySet ++ stgstates.keySet).map(k => {
        k -> join(m.get(k), stgstates.get(k))
      }).toMap
    })
  }

  /**
   * If any stage both calls "start" and "end" for a lock region,
   * then this function will eliminate both of them since we know statically
   * that the region is a single stage and therefore mutually exclusive.
   * @param stg The stage to modify
   */
  def eliminateLockRegions(stg: PStage): Unit = {
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
  /**
   * This takes a memory identifier and a set of lock operations
   * for that identifier. If certain combinations of operations
   * are present, this returns a condensed set of new operations.
   * Otherwise, they are left unmodified.
   * @param mod The memory that is being considered
   * @param lops A set of lock operations relevant to mod
   * @return A new merged set of lock operations if any merges apply
   */
  def mergeLockOps(mod: LockArg, lops: Iterable[Command]): Iterable[Command] = {
    //res + rel -> checkfree
    //res + checkowned -> res + checkfree
    //else same
    val rescmd = lops.find {
      case _:IReserveLock => true
      case _ => false
    }
    val relcmd = lops.find {
      case _:IReleaseLock => true
      case _ => false
    }
    val checkownedCmd = lops.find {
      case _:ICheckLockOwned => true
      case _ => false
    }
    if (rescmd.isDefined && relcmd.isDefined) {
      List(ICheckLockFree(mod))
    } else if (rescmd.isDefined && checkownedCmd.isDefined) {
      List(rescmd.get, ICheckLockFree(mod))
    } else {
      lops
    }
  }

  /**
   * Define common helper methods implicit classes.
   */
  implicit class RichLock(l: LockState) {
    def matchOrError[A](pos: Position, name: String, exp: LockState)
      (andThen: PartialFunction[LockState, A]): A = {
      val mismatchError: PartialFunction[LockState, A] = {
        case _ => throw InvalidLockState(pos, name, l, exp)
      }
      andThen.orElse(mismatchError)(l)
    }
  }
}
