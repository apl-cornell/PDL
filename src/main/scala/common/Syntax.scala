package pipedsl.common
import scala.util.parsing.input.{Positional, Position}
import Errors._
import common.Security._

object Syntax {
  /**
   * Annotations added by the various passes of the type checker.
   */
  object Annotations {

    sealed trait TypeAnnotation {
      var typ: Option[Type] = None;
    }
    sealed trait LabelAnnotation {
      var lbl: Option[Label] = None;
    }
  }

  object OpConstructor {
    val add: (Double, Double) => Double = (_ + _)
    val mul: (Double, Double) => Double = (_ * _)
    val div: (Double, Double) => Double  = (_ / _)
    val sub: (Double, Double) => Double = (_ - _)
    val mod: (Double, Double) => Double  = (_ % _)
    val band: (Long, Long) => Long = (_ & _)
    val bor: (Long, Long) => Long = (_ | _)
    val bxor: (Long, Long) => Long = (_ ^ _)
    val sl: (Long, Long) => Long = (_ << _)
    val sr: (Long, Long) => Long = (_ >> _)
    val or: (Boolean, Boolean) => Boolean = (_ || _)
    val and: (Boolean, Boolean) => Boolean = (_ && _)
  }

  import Annotations._

  case class Id(v: String) extends Positional with TypeAnnotation with LabelAnnotation {
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
    def apply(v1: Any, v2: Any): Option[Any] = this match {
      case n: NumOp => Some(n.fun(v1.asInstanceOf[Number].doubleValue(),
        v2.asInstanceOf[Number].doubleValue()))
      case b: BitOp => Some(b.fun(v1.asInstanceOf[Number].longValue(),
        v2.asInstanceOf[Number].longValue()))
      case b: BoolOp => Some(b.fun(v1.asInstanceOf[Boolean], v2.asInstanceOf[Boolean]))
      case _ => None
    }
  }

  case class EqOp(op: String) extends BOp
  case class CmpOp(op: String) extends BOp
  case class BoolOp(op: String, fun: (Boolean, Boolean) => Boolean) extends BOp
  case class NumOp(op: String, fun: (Double, Double) => Double) extends BOp
  case class BitOp(op: String, fun: (Long, Long) => Long) extends BOp

  sealed trait Expr extends Positional with TypeAnnotation {
    def isLVal = this match {
      case _:EVar => true
      case _:EMemAccess => true
      case _ => false
    }
  }
  case class EInt(v: Int, base: Int = 10) extends Expr
  case class ERational(d: String) extends Expr
  case class EBool(v: Boolean) extends Expr
  case class EBinop(op: BOp, e1: Expr, e2: Expr) extends Expr
  case class ERecAccess(rec: Expr, fieldName: Id) extends Expr
  case class ERecLiteral(fields: Map[Id, Expr]) extends Expr
  case class EMemAccess(mem: Expr, index: Expr) extends Expr
  case class EBitExtract(num: Expr, start: Expr, end: Expr) extends Expr
  case class ETernary(cond: Expr, tval: Expr, fval: Expr) extends Expr
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
  case class CRecv(lhs: Expr, rhs: Expr) extends Command {
    if (lhs.isLVal == false) throw UnexpectedLVal(lhs, "assignment")
  }
  case class CCall(id: Id, args: List[Expr]) extends Command
  case class COutput(exp: Expr) extends Command
  case class CExpr(exp: Expr) extends Command
  case object CEmpty extends Command

  sealed trait Definition extends Positional

  case class FuncDef(
    name: Id,
    args: List[Param],
    ret: Type,
    body: Command) extends Definition

  case class PipeDef(
    name: Id,
    input: List[Param],
    modules: List[Id], //TODO external module connections
    body: Command) extends Definition

  case class Param(name: Id, typ: Type) extends Positional

}
