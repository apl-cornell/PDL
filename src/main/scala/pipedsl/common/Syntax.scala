package pipedsl.common
import scala.util.parsing.input.{Position, Positional}
import Errors._
import Security._
import pipedsl.common.LockImplementation.LockInterface
import pipedsl.common.Locks.{General, LockGranularity, LockState}
import com.microsoft.z3.BoolExpr


object Syntax {
  /**
   * Annotations added by the various passes of the type checker.
   */
  object Annotations {
    sealed trait TypeAnnotation {
      var typ: Option[Type] = None
    }
    sealed trait LabelAnnotation {
      var lbl: Option[Label] = None
    }
    sealed trait RecursiveAnnotation {
      var isRecursive: Boolean = true
    }
    sealed trait SpeculativeAnnotation {
      var maybeSpec: Boolean = false
    }
    sealed trait LockInfoAnnotation {
      var memOpType: Option[LockType] = None
      var granularity: LockGranularity = General
    }
    sealed trait SMTPredicate {
      var predicateCtx: Option[BoolExpr] = None
    }
    sealed trait PortAnnotation
    {
      var portNum :Option[Int] = None
    }
  }

  object Latency extends Enumeration {
    type Latency = Value
    val Combinational: Latency = Value("c")
    val Sequential: Latency = Value("s")
    val Asynchronous: Latency = Value("a")

    def join(l1: Latency, l2: Latency): Latency = l1 match {
      case Combinational => l2
      case Sequential => l2 match {
        case Asynchronous => Asynchronous
        case _ => l1
      }
      case Asynchronous => l1
    }
  }

  import Latency._

  object RequestType extends Enumeration {
    type RequestType = Value
    val Lock, Module, Speculation = Value
  }

  import RequestType._

  object OpConstructor {
    val add: (Int, Int) => Int = (_ + _)
    val mul: (Int, Int) => Int = (_ * _)
    val div: (Int, Int) => Int  = (_ / _)
    val sub: (Int, Int) => Int = (_ - _)
    val mod: (Int, Int) => Int  = (_ % _)
    val band: (Int, Int) => Int = (_ & _)
    val bor: (Int, Int) => Int = (_ | _)
    val bxor: (Int, Int) => Int = (_ ^ _)
    val sl: (Int, Int) => Int = (_ << _)
    val sr: (Int, Int) => Int = (_ >> _)
    val or: (Boolean, Boolean) => Boolean = (_ || _)
    val and: (Boolean, Boolean) => Boolean = (_ && _)
    val concat: (Int, Int) => Int = (a, b) => (a << (32-Integer.numberOfLeadingZeros(b)) | b)
  }

  import Annotations._

  case class Id(v: String) extends Positional with TypeAnnotation {
    override def toString = s"$v"
  }

  sealed trait Type extends Positional with LabelAnnotation with SpeculativeAnnotation {
    override def toString: String = this match {
      case _: TVoid => "void"
      case _: TBool => "bool"
      case _: TString => "String"
      case TSizedInt(l, un) => s"${if (un) "u" else ""}bit<$l>"
      case TFun(args, ret) => s"${args.mkString("->")} -> ${ret}"
      case TRecType(n, _) => s"$n"
      case TMemType(elem, size, rLat, wLat, rPorts, wPorts) =>
        s"${elem.toString}[${size}]<$rLat$rPorts, $wLat$wPorts>"
      case TLockedMemType(m, sz, impl) => s"${m.toString}(${impl.toString})".concat(
        if (sz.isDefined) s"<${sz.get.toString}>" else "")
      case TModType(ins, refs, _, _) => s"${ins.mkString("->")} ++ ${refs.mkString("=>")})"
      case TRequestHandle(m, _) => s"${m}_Request"
      case TMaybe(btyp) => s"Maybe<${btyp}>"
      case TNamedType(n) => n.toString
      case TBitWidthAdd(b1, b2) => "add(" + b1 + ", " + b2 + ")"
      case TBitWidthLen(len) => len.toString()
      case TBitWidthMax(b1, b2) => "max(" + b1 + ", " + b2 + ")"
      case TBitWidthVar(name) => "bitVar(" + name + ")"

    }
  }
  // Types that can be upcast to Ints
  sealed trait IntType
  case class TSizedInt(len: TBitWidth, unsigned: Boolean) extends Type with IntType
  // Use case class instead of case object to get unique positions
  case class TString() extends Type
  case class TVoid() extends Type
  case class TBool() extends Type
  case class TFun(args: List[Type], ret: Type) extends Type
  case class TRecType(name: Id, fields: Map[Id, Type]) extends Type
  case class TMemType(elem: Type,
                      addrSize: Int,
                      readLatency: Latency = Latency.Asynchronous,
                      writeLatency: Latency = Latency.Asynchronous,
                      readPorts: Int,
                      writePorts: Int) extends Type
  case class TModType(inputs: List[Type], refs: List[Type], retType: Option[Type], name: Option[Id] = None) extends Type
  case class TLockedMemType(mem: TMemType, idSz: Option[Int], limpl: LockInterface) extends Type
  case class TRequestHandle(mod: Id, rtyp: RequestType) extends Type
  //This is primarily used for parsing and is basically just a type variable
  case class TNamedType(name: Id) extends Type
  case class TMaybe(btyp: Type) extends Type
  sealed trait TBitWidth extends Type
  case class TBitWidthVar(name: Id) extends TBitWidth
  case class TBitWidthLen(len: Int) extends TBitWidth
  case class TBitWidthAdd(b1: TBitWidth, b2: TBitWidth) extends TBitWidth
  case class TBitWidthMax(b1: TBitWidth, b2: TBitWidth) extends TBitWidth


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

  sealed trait UOp extends Positional {
    val op: String;
    override def toString: String = this.op
    def operate(v1: Any): Option[Any] = this match {
      //TODO match on all case UOps, currently seems like only BoolUOp is actually parsed
      case b:BoolUOp => b.op match {
        case "!" => Some(!v1.asInstanceOf[Boolean])
        case _ => None
      }
      case _ => throw new UnsupportedOperationException
    }
  }
  
  case class BoolUOp(op: String) extends UOp
  case class NumUOp(op: String) extends UOp
  case class BitUOp(op: String) extends UOp

  def NegOp(): NumUOp = NumUOp("-")
  def NotOp(): BoolUOp = BoolUOp("!")
  def MagOp(): NumUOp = NumUOp("abs")
  def SignOp(): NumUOp = NumUOp("signum")
  def AndOp(e1: Expr,e2: Expr): EBinop = EBinop(BoolOp("&&", OpConstructor.and), e1,e2)
  def OrOp(e1: Expr, e2: Expr): EBinop = EBinop(BoolOp("||", OpConstructor.or), e1, e2)
  def EqOp(e1: Expr, e2: Expr): EBinop = EBinop(EqOp("=="), e1, e2)

  sealed trait BOp extends Positional {
    val op: String;
    override def toString = this.op
    def operate(v1: Any, v2: Any): Option[Any] = this match {
      case n: NumOp => Some(n.fun(v1.asInstanceOf[Number].intValue(),
        v2.asInstanceOf[Number].intValue()))
      case b: BitOp => Some(b.fun(v1.asInstanceOf[Number].intValue(),
        v2.asInstanceOf[Number].intValue()))
      case b: BoolOp => Some(b.fun(v1.asInstanceOf[Boolean], v2.asInstanceOf[Boolean]))
      case e: EqOp => e.op match {
        case "==" => Some(v1 == v2)
        case "!=" => Some(v1 != v2)
        case _ => None
      }
      case c: CmpOp => c.op match {
        case ">" => Some(v1.asInstanceOf[Int] > v2.asInstanceOf[Int])
        case ">=" => Some(v1.asInstanceOf[Int] >= v2.asInstanceOf[Int])
        case "<=" => Some(v1.asInstanceOf[Int] <= v2.asInstanceOf[Int])
        case "<" => Some(v1.asInstanceOf[Int] < v2.asInstanceOf[Int])
        case _ => None
      }
      case _ => None
    }
  }

  case class EqOp(op: String) extends BOp
  case class CmpOp(op: String) extends BOp
  case class BoolOp(op: String, fun: (Boolean, Boolean) => Boolean) extends BOp
  case class NumOp(op: String, fun: (Int, Int) => Int) extends BOp
  case class BitOp(op: String, fun: (Int, Int) => Int) extends BOp

  sealed trait Expr extends Positional with TypeAnnotation with PortAnnotation {
    def isLVal = this match {
      case _:EVar => true
      case _:EMemAccess => true
      case _ => false
    }
    def copyMeta(from: Expr): Expr = {
      setPos(from.pos)
      typ = from.typ
      this
    }
  }
  
  case class LockArg(id: Id, evar: Option[EVar]) extends Positional with LockInfoAnnotation
  sealed trait LockType extends Positional
  case object LockRead extends LockType
  case object LockWrite extends LockType
  case object EInvalid extends Expr
  case class EIsValid(ex: Expr) extends Expr
  case class EFromMaybe(ex: Expr) extends Expr
  case class EToMaybe(ex: Expr) extends Expr
  case class EInt(v: Int, base: Int = 10, bits: Int = 32) extends Expr
  case class EString(v: String) extends Expr
  case class EBool(v: Boolean) extends Expr
  case class EUop(op: UOp, ex: Expr) extends Expr
  case class EBinop(op: BOp, e1: Expr, e2: Expr) extends Expr
  case class ERecAccess(rec: Expr, fieldName: Id) extends Expr
  case class ERecLiteral(fields: Map[Id, Expr]) extends Expr
  case class EMemAccess(mem: Id, index: Expr, wmask: Option[Expr] = None) extends Expr with LockInfoAnnotation
  case class EBitExtract(num: Expr, start: Int, end: Int) extends Expr
  case class ETernary(cond: Expr, tval: Expr, fval: Expr) extends Expr
  case class EApp(func: Id, args: List[Expr]) extends Expr
  case class ECall(mod: Id, args: List[Expr]) extends Expr
  case class EVar(id: Id) extends Expr
  case class ECast(ctyp: Type, exp: Expr) extends Expr


  sealed trait Command extends Positional with SMTPredicate with PortAnnotation
  case class CSeq(c1: Command, c2: Command) extends Command
  case class CTBar(c1: Command, c2: Command) extends Command
  case class CIf(cond: Expr, cons: Command, alt: Command) extends Command
  case class CAssign(lhs: EVar, rhs: Expr, typ: Option[Type]) extends Command{
    if (!lhs.isLVal) throw UnexpectedLVal(lhs, "assignment")
  }
  case class CRecv(lhs: Expr, rhs: Expr, typ: Option[Type]) extends Command {
    if (!lhs.isLVal) throw UnexpectedLVal(lhs, "assignment")
  }
  case class CSpecCall(handle: EVar, pipe: Id, args: List[Expr]) extends Command
  case class CCheckSpec(isBlocking: Boolean) extends Command
  case class CVerify(handle: EVar, args: List[Expr], preds: List[Expr]) extends Command
  case class CInvalidate(handle: EVar) extends Command
  case class CPrint(args: List[Expr]) extends Command
  case class COutput(exp: Expr) extends Command
  case class CReturn(exp: Expr) extends Command
  case class CExpr(exp: Expr) extends Command
  case class CLockStart(mod: Id) extends Command
  case class CLockEnd(mod: Id) extends Command
  case class CLockOp(mem: LockArg, op: LockState, var lockType: Option[LockType]) extends Command with LockInfoAnnotation
  case class CSplit(cases: List[CaseObj], default: Command) extends Command
  case class CEmpty() extends Command


  sealed trait InternalCommand extends Command

  case class ICondCommand(cond: Expr, cs: List[Command]) extends InternalCommand
  case class IUpdate(specId: Id, value: EVar, originalSpec: EVar) extends InternalCommand
  case class ICheck(specId: Id, value: EVar) extends InternalCommand
  case class ISend(handle: EVar, receiver: Id, args: List[EVar]) extends InternalCommand
  case class IRecv(handle: EVar, sender: Id, result: EVar) extends InternalCommand
  //TODO Clean up what actually needs the lock info annotation
  case class IMemSend(handle: EVar, writeMask: Option[Expr], mem: Id, data: Option[EVar], addr: EVar)
    extends InternalCommand with LockInfoAnnotation {
    def isWrite: Boolean = data.isDefined
  }
  case class IMemRecv(mem: Id, handle: EVar, data: Option[EVar]) extends InternalCommand with LockInfoAnnotation
  //used for sequential memories that don't commit writes immediately

  case class IMemWrite(mem: Id, addr: EVar, data: EVar) extends InternalCommand with LockInfoAnnotation
  case class ICheckLockFree(mem: LockArg) extends InternalCommand with LockInfoAnnotation
  case class ICheckLockOwned(mem: LockArg, handle: EVar) extends InternalCommand with LockInfoAnnotation
  case class IReserveLock(handle: EVar, mem: LockArg) extends InternalCommand with LockInfoAnnotation
  case class IAssignLock(handle: EVar, src: Expr, default: Option[Expr]) extends InternalCommand with LockInfoAnnotation
  case class IReleaseLock(mem: LockArg, handle: EVar) extends InternalCommand with LockInfoAnnotation
  //needed for internal compiler passes to track branches with explicitly no lockstate change
  case class ILockNoOp(mem: LockArg) extends InternalCommand

  case class CaseObj(cond: Expr, body: Command) extends Positional

  sealed trait Definition extends Positional

  case class FuncDef(
    name: Id,
    args: List[Param],
    ret: Type,
    body: Command) extends Definition

  case class ModuleDef(
    name: Id,
    inputs: List[Param],
    modules: List[Param],
    ret: Option[Type],
    body: Command) extends Definition with RecursiveAnnotation with SpeculativeAnnotation

  case class Param(name: Id, typ: Type) extends Positional

  case class Prog(
          fdefs: List[FuncDef],
          moddefs: List[ModuleDef],
          circ: Circuit) extends Positional

  sealed trait Circuit extends Positional
  case class CirSeq(c1: Circuit, c2: Circuit) extends Circuit
  case class CirConnect(name: Id, c: CirExpr) extends Circuit
  case class CirExprStmt(ce: CirExpr) extends Circuit

  sealed trait CirExpr extends Expr
  case class CirMem(elemTyp: Type, addrSize: Int, numPorts: Int) extends CirExpr
  case class CirRegFile(elemTyp: Type, addrSize: Int) extends CirExpr
  //TODO do these ever need other kinds of parameters besides ints?
  //this allows us to build a "locked" version of a memory
  case class CirLock(mem: Id, impl: LockInterface, szParams: List[Int]) extends CirExpr
  //This is an already "locked" memory (i.e. one line instantiation, no reference to the unlocked memory)
  case class CirLockMem(elemTyp: Type, addrSize: Int, impl: LockInterface, szParams: List[Int], numPorts: Int) extends CirExpr
  case class CirLockRegFile(elemTyp: Type, addrSize: Int, impl: LockInterface, szParams: List[Int]) extends CirExpr
  case class CirNew(mod: Id, mods: List[Id]) extends CirExpr
  case class CirCall(mod: Id, args: List[Expr]) extends CirExpr
}
