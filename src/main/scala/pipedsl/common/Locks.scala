package pipedsl.common

import pipedsl.common.DAGSyntax.PStage
import pipedsl.common.Dataflow.DFMap
import pipedsl.common.Errors.InvalidLockState
import pipedsl.common.Syntax.{CLockOp, Id}

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


  def transferLockStates(node: PStage, instates: Map[Id, LockState]): Map[Id, LockState] = {
    var newMap = instates
    node.getCmds.foreach {
      //TODO Unique locks update
      case CLockOp(mem, op) => newMap = newMap.updated(mem.id, op)
      case _ => ()
    }
    newMap
  }

  //Need the node argument to fit the Dataflow signature
  def mergeLockStates(node: PStage, instates: DFMap[Map[Id, LockState]]): Map[Id, LockState] = {
    instates.keys.foldLeft(Map[Id, LockState]())((m, stg) => {
      val stgstates = instates(stg)
      (m.keySet ++ stgstates.keySet).map(k => {
        k -> join(m.get(k), stgstates.get(k))
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
