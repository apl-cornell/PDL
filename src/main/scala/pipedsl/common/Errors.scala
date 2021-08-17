package pipedsl.common

import scala.util.parsing.input.{NoPosition, Position, Positional}
import Syntax._
import pipedsl.common.Locks.LockState

object Errors {
  def withPos(s: String, pos: Position, postMsg: String = "") =
    s"[${pos.line}.${pos.column}] $s\n${pos.longString}\n${postMsg}"

  class TypeError(msg: String,
          pos: Position,
          postMsg: String) extends RuntimeException(withPos(msg, pos, postMsg)) {
    def this(msg: String, pos: Position) = this(msg, pos, "")
  }

  case class BaseError(b: Int) extends RuntimeException(
    s"Base $b is not supported"
  )

  // Parsing errors
  case class ParserError(msg: String) extends RuntimeException(msg)

  case class UnexpectedLVal(e: Expr, construct: String) extends RuntimeException(
    withPos(s"Expected L-value in $construct.", e.pos))

  case class UnexpectedExpr(e: Expr) extends RuntimeException(
    withPos(s"Cannot interpret $e", e.pos)
  )
  case class UnexpectedCommand(c: Command) extends RuntimeException(
    withPos(s"Reached unexpected command $c", c.pos)
  )
  case class UnexpectedLockImpl(i: Id) extends RuntimeException(
    withPos(s"$i is not a valid lock implementation name", i.pos)
  )

  case class InvalidBitExtraction(s: Int, e: Int) extends RuntimeException(
    s"Start: $s must be less than or equal to End: $e"
  )
  case class AlreadySetException(id: Id) extends RuntimeException(
    s"Tried to set variable $id twice.")

  case class MalformedFunction(pos: Position, msg: String) extends RuntimeException(
    withPos(s"Bad Function Definition: $msg", pos)
  )

  case class UnexpectedCase(pos: Position) extends RuntimeException(
    withPos(s"Analysis found unexpected case", pos)
  )

  case class UnexpectedPipelineStatement(pos: Position, msg: String) extends RuntimeException(
    withPos(s"Tried to execute a $msg statement inside of a conditional block", pos)
  )

  case class UnexpectedCall(pos: Position) extends RuntimeException(
    withPos(s"Module should have at most 1 call statement through execution path", pos)
  )

  case class UnexpectedReturn(pos: Position) extends RuntimeException(
    withPos(s"Only function bodies should contain return statements", pos)
  )

  case class MismatchedAssigns(pos: Position, vars: Set[Id]) extends RuntimeException(
    withPos(s"Both branches of if statement did not assign to ${vars.mkString(",")}", pos)
  )

  case class UnexpectedAssignment(pos: Position, id: Id) extends RuntimeException(
    withPos(s"Variable $id has already been assigned", pos)
  )

  case class IllegalTypeMerge(pos: Position, id: Id, ltyp: Type, rtyp: Type) extends TypeError(
    s"Type of $id cannot be both $ltyp and $rtyp", pos)

  case class IllegalLockModification(pos: Position, n: String,
    oldstate: LockState, newstate: LockState) extends TypeError(
    s"Tried to illegally modify state of lock $n from $oldstate to $newstate", pos)

  case class IllegalLockMerge(pos: Position, n: String, l: LockState, r:LockState) extends TypeError(
    s"Cannot merge lock states {$l, $r} across branch for variable $n", pos)

  case class InvalidLockState(pos: Position, name: String, l: LockState, exp: LockState) extends TypeError(
    s"Invalid lock state for $name, was $l, expected $exp", pos)

  case class IllegalLockAcquisition(pos: Position) extends TypeError(
    s"Cannot acquire or reserve locks inside multiple branches", pos)

  case class IllegalOOOLockRelease(pos: Position) extends TypeError(
    s"Cannot release locks inside multiple branches", pos)

  case class IllegalLockRelease(pos: Position) extends TypeError(
    s"Cannot release locks inside of a speculative block", pos)

  case class IllegalCast(pos: Position, rtyp: Type, ctyp: Type) extends TypeError(
    s"Cannot cast type $rtyp to $ctyp", pos)

  case class AlreadyBoundType(pos: Position, node: String, oldT: Type, newT: Type) extends TypeError(
    s"Tried to bind type: $newT to $node but already has type: $oldT", pos)

  case class ArgLengthMismatch(pos: Position, arglen: Int, typlen: Int) extends TypeError(
    s"Expected $typlen arguments, got $arglen", pos)

  case class UnexpectedType(pos: Position, node: String, exp: String, actual: Type) extends TypeError(
    s"Expected type $exp in $node, received: $actual.", pos)

  case class UnexpectedSubtype(pos: Position, node: String, exp: Type, actual: Type) extends TypeError(
    s"Expected subtype of $exp in $node, received: $actual.", pos)

  case class MissingType(pos: Position, node: String) extends TypeError(
    s"No type found for $node", pos)

  case class UnavailableArgUse(pos: Position, node: String) extends TypeError(
    s"$node used when not yet available", pos)

  case class UnexpectedAsyncReference(pos: Position, node: String) extends TypeError (
    s"$node references memory in a synchronous statement", pos)

  case class UnexpectedSyncReference(pos: Position, msg: String) extends TypeError (
    msg, pos
  )
  case class UnresolvedSpeculation(pos: Position) extends TypeError (
    s"Speculation was never resolved", pos)

  case class AlreadyResolvedSpeculation(pos: Position) extends TypeError(
    s"Redundant resolve operation, not possible to be speculative here", pos)

  case class MismatchedSpeculationState(pos: Position) extends TypeError(
    s"All execution branches must resolve in the same speculation state", pos)

  case class IllegalSpeculationBlock(pos: Position) extends RuntimeException(
    withPos("Speculation initiation and verification must be in separate stages", pos)
  )

  case class IllegalSpeculativeOperation(pos: Position, exp: String) extends TypeError(
    s"Operation cannot be performed speculatively, must be in speculative state: ${exp}", pos
  )

  case class IllegalBSVStage(msg: String) extends RuntimeException(msg)

  case class UnexpectedBSVType(msg: String) extends RuntimeException(msg)
  
  case class MalformedLockTypes(msg: String) extends RuntimeException(msg)

  case class IllegalMemoryAccessOperation(pos: Position) extends TypeError(
    s"Memory access's associated lock must have the correct READ or WRITE capabilities", pos
  )

  case class MissingPredictionValues(pos: Position, handle: String) extends TypeError(
    s"Cannot find the predicted values for speculation ${handle}", pos
  )

  case class InsufficientPorts(
    pos :Position,
    portType :String,
    memory :Syntax.Id,
    found :Int,
    required :Int) extends RuntimeException(
    withPos(s"Not enough ${portType} ports on $memory. Required $required but found $found.", pos))

  case class SuboptimalPorts(
    pos :Position,
    portType :String,
    memory :Syntax.Id,
    found :Int,
    required :Int) extends RuntimeException(
    withPos(s"Better throughput could be obtained by having " +
      s"$required $portType ports on $memory instead of $found.", pos))

  case class NoSuperScalar(pipe :Syntax.Id) extends RuntimeException(
    withPos(s"Tried to call $pipe multiple times in the same cycle!", pipe.pos))

  case class LonelyPaths(pipe :Syntax.Id) extends RuntimeException(
    withPos(s"There are program paths that do not have output nor call in $pipe!", pipe.pos))

  case class MultipleCall(pos :Position, sure :Boolean) extends RuntimeException(
    withPos(if (sure) "A thread potentially has more than one output xor call!"
    else "The solver is confused. A thread may have more than one output xor call", pos))

  case class TooManyPorts(pos :Position, found :Int) extends RuntimeException(
    withPos(s"Too many ports! Found $found", pos)
  )

  case class UnificationError(t1: Type, t2: Type) extends RuntimeException(
    withPos(s"Unable to unify type $t1 and type $t2", t1.pos)
  )

  case class TypeMeetError(t1 :Type, t2 :Type) extends RuntimeException(
    withPos(s"Cannot generate meet of type $t1 and type $t2", if (t1.pos eq NoPosition) t2.pos else t1.pos)
  )

  case class LackOfConstraints(e :Positional) extends RuntimeException(
    withPos(s"Not enough constraints provided to infer types. Found error at $e", e.pos)
  )

  case class IntWidthNotSpecified() extends RuntimeException
}
