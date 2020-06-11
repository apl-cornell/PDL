package pipedsl.common

import Errors.InvalidLockState
import Locks.LockState.LockState
import scala.util.parsing.input.Position

object Locks {

  object LockState extends Enumeration {
    type LockState = Value
    val Free, Reserved, Acquired, Released = Value
  }


  /**
   * Define common helper methods implicit classes.
   */
  implicit class RichLock(l: LockState) {
    def matchOrError[A](pos: Position, exp: LockState)
      (andThen: PartialFunction[LockState, A]): A = {
      val mismatchError: PartialFunction[LockState, A] = {
        case _ => throw InvalidLockState(pos, l, exp)
      }
      andThen.orElse(mismatchError)(l)
    }
  }
}
