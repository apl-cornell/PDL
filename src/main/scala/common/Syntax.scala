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
    val band: (Int, Int) => Int = (_ & _)
    val bor: (Int, Int) => Int = (_ | _)
    val bxor: (Int, Int) => Int = (_ ^ _)
    val sl: (Int, Int) => Int = (_ << _)
    val sr: (Int, Int) => Int = (_ >> _)
    val or: (Boolean, Boolean) => Boolean = (_ || _)
    val and: (Boolean, Boolean) => Boolean = (_ && _)
    val concat: (Int, Int) => Int = (_ + _) //TODO fix for interpreter
  }

  import Annotations._

  case class Id(v: String) extends Positional with TypeAnnotation with LabelAnnotation {
    override def toString = s"$v"
  }

  sealed trait Type extends Positional {
    override def toString = this match {
      case _: TVoid => "void"
      case _: TBool => "bool"
      case TFixed(t,i, un) => s"${if (un) "u" else ""}fix<$t,$i>"
      case TSizedInt(l, un) => s"${if (un) "u" else ""}bit<$l>"
      case TStaticInt(s) => s"static($s)"
      case TFun(args, ret) => s"${args.mkString("->")} -> ${ret}"
      case TRecType(n, _) => s"$n"
      case TAlias(n) => n.toString
    }
  }
  // Types that can be upcast to Ints
  sealed trait IntType
  case class TSizedInt(len: Int, unsigned: Boolean) extends Type with IntType
  case class TStaticInt(v: Int) extends Type with IntType
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

  /**
   * Define common helper methods implicit classes.
   */
  implicit class RichType(typ: Type) {
    def matchOrError[A](pos: Position, construct: String, exp: String)
            (andThen: PartialFunction[Type, A]): A = {
      val mismatchError: PartialFunction[Type, A] = {
        case _ => throw UnexpectedType(pos, construct, exp, typ)
      }
      andThen.orElse(mismatchError)(typ)
    }
  }

  sealed trait BOp extends Positional {
    val op: String;
    override def toString = this.op
    def operate(v1: Any, v2: Any): Option[Any] = this match {
      case n: NumOp => Some(n.fun(v1.asInstanceOf[Number].doubleValue(),
        v2.asInstanceOf[Number].doubleValue()))
      case b: BitOp => Some(b.fun(v1.asInstanceOf[Number].intValue(),
        v2.asInstanceOf[Number].intValue()))
      case b: BoolOp => Some(b.fun(v1.asInstanceOf[Boolean], v2.asInstanceOf[Boolean]))
      case e: EqOp => e.op match {
        case "==" => Some(v1 == v2)
        case "!=" => Some(v1 != v2)
        case _ => None
      }
      case c: CmpOp => c.op match {
        case ">" => Some(1.asInstanceOf[Int] > v2.asInstanceOf[Int])
        case ">=" => Some(1.asInstanceOf[Int] >= v2.asInstanceOf[Int])
        case "<=" =>  Some(1.asInstanceOf[Int] <= v2.asInstanceOf[Int])
        case "<" => Some(1.asInstanceOf[Int] < v2.asInstanceOf[Int])
        case _ => None
      }
      case _ => None
    }
  }

  case class EqOp(op: String) extends BOp
  case class CmpOp(op: String) extends BOp
  case class BoolOp(op: String, fun: (Boolean, Boolean) => Boolean) extends BOp
  case class NumOp(op: String, fun: (Double, Double) => Double) extends BOp
  case class BitOp(op: String, fun: (Int, Int) => Int) extends BOp

  sealed trait Expr extends Positional with TypeAnnotation {
    def isLVal = this match {
      case _:EVar => true
      case _:EMemAccess => true
      case _ => false
    }
  }
  case class EInt(v: Int, base: Int = 10, bits: Int = 32) extends Expr
  case class EBool(v: Boolean) extends Expr
  case class EBinop(op: BOp, e1: Expr, e2: Expr) extends Expr
  case class ERecAccess(rec: Expr, fieldName: Id) extends Expr
  case class ERecLiteral(fields: Map[Id, Expr]) extends Expr
  case class EMemAccess(mem: Expr, index: Expr) extends Expr
  case class EBitExtract(num: Expr, start: Expr, end: Expr) extends Expr
  case class ETernary(cond: Expr, tval: Expr, fval: Expr) extends Expr
  case class EApp(func: Id, args: List[Expr]) extends Expr
  case class EVar(id: Id) extends Expr

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
  case class CReturn(exp: Expr) extends Command
  case class CExpr(exp: Expr) extends Command
  case object CEmpty extends Command

  sealed trait Definition extends Positional

  case class FuncDef(
    name: Id,
    args: List[Param],
    ret: Type,
    body: Command) extends Definition

  case class ModuleDef(
    name: Id,
    input: List[Param],
    modules: List[Param], //TODO external module connections
    body: Command) extends Definition

  case class Param(name: Id, typ: Type) extends Positional

  case class Prog(
          fdefs: List[FuncDef],
          pipedefs: List[ModuleDef],
          circ: Circuit) extends Positional

  sealed trait Circuit extends Positional

  case class CirMem(addrSize: Int, typ: Type) extends Circuit
  case class CirSeq(c1: Circuit, c2: Circuit) extends Circuit
  case class CirNew(mod: Id, inits: List[Expr], mods: List[Id]) extends Circuit
  case class CirName(name: Id, c: Circuit) extends Circuit

}
