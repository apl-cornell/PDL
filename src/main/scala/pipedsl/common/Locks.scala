package pipedsl.common

import Errors.InvalidLockState
import Locks.LockState.LockState
import pipedsl.common.DAGSyntax.PStage
import pipedsl.common.Dataflow.DFMap
import pipedsl.common.Syntax.{CLockOp, Id}

import scala.util.parsing.input.Position

object Locks {

  object LockState extends Enumeration {
    type LockState = Value
    //Definition order defines the < ordering
    //This order defines valid transition orders
    //I.e., locks are only allowed to grow
    val Free, Reserved, Acquired, Released = Value
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
  }


  def transferLockStates(node: PStage, instates: Map[Id, LockState]): Map[Id, LockState] = {
    var newMap = instates
    node.cmds.foreach {
      case CLockOp(mem, op) => newMap = newMap.updated(mem, op)
      case _ => ()
    }
    newMap
  }

  def mergeLockStates(node: PStage, instates: DFMap[Map[Id, LockState]]): Map[Id, LockState] = {
    instates.keys.foldLeft(Map[Id, LockState]())((m, stg) => {
      val stgstates = instates(stg)
      (m.keySet ++ stgstates.keySet).map(k => {
        (k -> LockState.join(m.get(k), stgstates.get(k)))
      }).toMap
    })
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
