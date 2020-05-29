package pipedsl.common

import scala.util.parsing.input.Position
import Syntax._
object Errors {
  def withPos(s: String, pos: Position, postMsg: String = "") =
    s"[${pos.line}.${pos.column}] $s\n${pos.longString}\n${postMsg}"

  class TypeError(msg: String,
          pos: Position,
          postMsg: String) extends RuntimeException(withPos(msg, pos, postMsg)) {
    def this(msg: String, pos: Position) = this(msg, pos, "")
  }

  // Parsing errors
  case class ParserError(msg: String) extends RuntimeException(msg)

  case class UnexpectedLVal(e: Expr, construct: String) extends RuntimeException(
    withPos(s"Expected L-value in $construct.", e.pos))

  case class UnexpectedExpr(e: Expr) extends RuntimeException(
    withPos(s"Cannot interpret $e", e.pos)
  )
  case class InvalidBitExtraction(s: Int, e: Int) extends RuntimeException(
    s"Start: $s must be less than or equal to End: $e"
  )
  case class AlreadySetException(id: Id) extends RuntimeException(
    s"Tried to set variable $id twice.")

  case class AlreadyBoundType(pos: Position, node: String, oldT: Type, newT: Type) extends TypeError(
    s"Tried to bind type: $newT to $node but already has type: $oldT", pos)

  case class UnexpectedType(pos: Position, node: String, exp: String, actual: Type) extends TypeError(
    s"Expected type $exp in $node, received: $actual.", pos)

  case class MissingType(pos: Position, node: String) extends TypeError(
    s"No type found for $node", pos)

}
