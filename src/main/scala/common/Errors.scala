package pipedsl.common

import scala.util.parsing.input.Position
import Syntax._
object Errors {
  def withPos(s: String, pos: Position, postMsg: String = "") =
    s"[${pos.line}.${pos.column}] $s\n${pos.longString}\n${postMsg}"

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
}
