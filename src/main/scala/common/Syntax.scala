package pipedsl.common
import scala.util.parsing.input.{Positional, Position}
import Errors._

object Syntax {
  /**
   * Annotations added by the various passes of the type checker.
   */
  object Annotations {

    sealed trait TypeAnnotation {
      var typ: Option[Type] = None;
    }
  }

  object OpConstructor {
    val add: (Long, Long) => Long = (_ + _)
    val mul: (Long, Long) => Long = (_ * _)
    val div: (Long, Long) => Long = (_ / _)
    val sub: (Long, Long) => Long = (_ - _)
    val mod: (Long, Long) => Long = (_ % _)
    val band: (Long, Long) => Long = (_ & _)
    val bor: (Long, Long) => Long = (_ | _)
    val bxor: (Long, Long) => Long = (_ ^ _)
    val sl: (Long, Long) => Long = (_ << _)
    val sr: (Long, Long) => Long = (_ >> _)
  }

  import Annotations._

  case class Id(v: String) extends Positional with TypeAnnotation {
    override def toString = s"$v"
  }

  sealed trait Type extends Positional {
    override def toString = this match {
      case _: TVoid => "void"
      case _: TBool => "bool"
      case _: TRational => "rational"
      case _: TFloat => "float"
      case _: TDouble => "double"
      case TFixed(t,i, un) => s"${if (un) "u" else ""}fix<$t,$i>"
      case TSizedInt(l, un) => s"${if (un) "u" else ""}bit<$l>"
      case TStaticInt(s) => s"static($s)"
      case TIndex(s, d) => s"idx($s, $d)"
      case TFun(args, ret) => s"${args.mkString("->")} -> ${ret}"
      case TRecType(n, _) => s"$n"
      case TAlias(n) => n.toString
    }
  }
  // Types that can be upcast to Ints
  sealed trait IntType
  case class TSizedInt(len: Int, unsigned: Boolean) extends Type with IntType
  case class TStaticInt(v: Int) extends Type with IntType
  case class TIndex(static: (Int, Int), dynamic: (Int, Int)) extends Type with IntType {
    // Our ranges are represented as s..e with e excluded from the range.
    // Therefore, the maximum value is one than the product of the interval ends.
    val maxVal: Int = static._2 * dynamic._2 - 1
  }
  // Use case class instead of case object to get unique positions
  case class TVoid() extends Type
  case class TBool() extends Type

  case class TRational(value:String) extends Type
  case class TFloat() extends Type
  case class TDouble() extends Type
  case class TFixed(ltotal:Int, lint:Int, unsigned:Boolean) extends Type
  case class TFun(args: List[Type], ret: Type) extends Type
  case class TRecType(name: Id, fields: Map[Id, Type]) extends Type
  case class TAlias(name: Id) extends Type

  sealed trait BOp extends Positional {
    val op: String;
    override def toString = this.op
    def toFun: Option[(Long, Long) => Long] = this match {
      case n: NumOp => Some(n.fun)
      case b: BitOp => Some(b.fun)
      case _ => None
    }
  }

  case class EqOp(op: String) extends BOp
  case class CmpOp(op: String) extends BOp
  case class BoolOp(op: String) extends BOp
  case class NumOp(op: String, fun: (Long, Long) => Long) extends BOp
  case class BitOp(op: String, fun: (Long, Long) => Long) extends BOp

  sealed trait Expr extends Positional with TypeAnnotation {
    def isLVal = this match {
      case _:EVar => true
      case _ => false
    }
  }
  case class EInt(v: Int, base: Int = 10) extends Expr
  case class ERational(d: String) extends Expr
  case class EBool(v: Boolean) extends Expr
  case class EBinop(op: BOp, e1: Expr, e2: Expr) extends Expr
  case class ERecAccess(rec: Expr, fieldName: Id) extends Expr
  case class ERecLiteral(fields: Map[Id, Expr]) extends Expr
  case class EApp(func: Id, args: List[Expr]) extends Expr
  case class EVar(id: Id) extends Expr
  case class ECast(e: Expr, castType: Type) extends Expr


  sealed trait Command extends Positional
  case class CSeq(c1: Command, c2: Command) extends Command
  case class CTBar(c1: Command, c2: Command) extends Command
  case class CIf(cond: Expr, cons: Command, alt: Command) extends Command
  case class CAssign(lhs: Expr, rhs: Expr) extends Command {
    if (lhs.isLVal == false) throw UnexpectedLVal(lhs, "assignment")
  }
  case class CReturn(exp: Expr) extends Command
  case class CExpr(exp: Expr) extends Command
  case object CEmpty extends Command
}
