package pipedsl.common
import scala.util.parsing.input.{Position, Positional}
import Errors._
import Security._
import pipedsl.common.LockImplementation.LockInterface
import pipedsl.common.Locks.{General, LockGranularity, LockState}
import com.microsoft.z3.BoolExpr
import pipedsl.common.Syntax.EIndConst
import pipedsl.common.Utilities.{generic_type_prefix, is_my_generic}
import pipedsl.typechecker.Subtypes

import scala.collection.mutable
import scala.language.implicitConversions





object Syntax {
  /**
   * Annotations added by the various passes of the type checker.
   */
  object Annotations {
    trait TypeAnnotation {
      var typ: Option[Type] = None

      def setType(t :Type) :this.type =
      {
        this.typ = Some(t)
        this
      }
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
    sealed trait Provisos
    {
      val adds: mutable.Map[(String, String), Id] = mutable.HashMap[(String, String), Id]()
      val mins: mutable.Map[String, Int] = mutable.HashMap[String, Int]()
      var constraints: List[Constraints.Constraint] = List()
    }
  }

  /**
   * forces us to implement .copyMeta() for things that should have it
   */
  sealed trait HasCopyMeta
  {
    val copyMeta : HasCopyMeta => HasCopyMeta = x => x
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
    val Lock, Module, Speculation, Checkpoint = Value
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
    def copyMeta(from :Id) :Id =
      {
        setPos(from.pos)
        typ = from.typ
        this
      }
  }

  sealed trait Type extends Positional with LabelAnnotation with SpeculativeAnnotation with HasCopyMeta {
    override def toString: String = this match {
      case _: TVoid => "void"
      case _: TBool => "bool"
      case _: TString => "String"
      case TSizedInt(l, un) => s"${if (un.unsigned()) "u" else ""}bit<$l>"
      case TFun(args, ret) => s"${args.mkString("->")} -> ${ret}"
      case TRecType(n, _) => s"$n"
      case TMemType(elem, size, rLat, wLat, rPorts, wPorts) =>
        s"${elem.toString}[${size}]<$rLat$rPorts, $wLat$wPorts>"
      case TLockedMemType(m, sz, impl) => s"${m.toString}(${impl.toString})".concat(
        if (sz.isDefined) s"<${sz.get.toString}>" else "")
      case TModType(ins, refs, _, _) => s"${ins.mkString("->")} ++ ${refs.mkString("=>")})"
      case TRequestHandle(m, _) => s"${m}_Request"
      case TReqHandle(tp, _) => s"${tp}_Request"
      case TMaybe(btyp) => s"Maybe<${btyp}>"
      case TNamedType(n) => n.toString
      case TBitWidthAdd(b1, b2) => "add(" + b1 + ", " + b2 + ")"
      case TBitWidthSub(b1, b2) => s"sub($b1, $b2)"
      case TBitWidthLen(len) => len.toString
      case TBitWidthMax(b1, b2) => "max(" + b1 + ", " + b2 + ")"
      case TBitWidthVar(name) => "bitVar(" + name + ")"
      case t :TSignedNess => t match
      {
        case TSigned() => "signed"
        case TUnsigned() => "unsigned"
        case TSignVar(name) => "sign(" + name + ")"
      }
      case TObject(name, tparams, methods) => s"$name <" + tparams + ">" +
        methods.foldLeft("")((s, f) => s + s"\n{${f._2._1.toString}}")
    }

    /**
     * function to copy metadata for types.
     * TODO: can we make this happen whenever .copy() is called? that would be really cool
     * if you want .copy() to work the way you might think it does you MUST do
     * a.copy(...).copyMeta(a);
     */
    override val copyMeta: HasCopyMeta => Type =
      {
        case from :Type =>
          setPos(from.pos)
          lbl = from.lbl
          maybeSpec = from.maybeSpec
          this
        case _ => this
      }

    /**
     * operator to calculate some semblance of a meet of this and that
     * hex code 2293 :)
     */
    def ⊓(that:Type) :Type = this match
    {
      case ness: TSignedNess => ness match
      {
        case TSignVar(id1) => that match
        {
          case TSignVar(id2) => if (id1.v != id2.v) throw TypeMeetError(this, that) else this
          case _ => that
        }
        case TSigned() => if (that.isInstanceOf[TUnsigned]) throw TypeMeetError(this, that) else this
        case TUnsigned() => if (that.isInstanceOf[TSigned]) throw TypeMeetError(this, that) else this
        case _ => throw TypeMeetError(this, that)
      }
      case TSizedInt(len1, sign1) => that match
      {
        case TSizedInt(len2, sign2) =>
          TSizedInt((len1 ⊓ len2).asInstanceOf[TBitWidth], (sign1 ⊓ sign2).asInstanceOf[TSignedNess])
        case TNamedType(_) => this
        case _ =>  throw TypeMeetError(this, that)
      }
      case TFun(args1, ret1) => that match {
        case TFun(args2, ret2) =>
          TFun(args1.zip(args2).map(t1t2 => t1t2._1 ⊓ t1t2._2), ret1 ⊓ ret2)
        case _ => throw TypeMeetError(this, that)
      }
      case _ :TRecType => if (this == that) this else throw TypeMeetError(this, that)
      case _ :TMemType =>
        if(this == that) this else throw TypeMeetError(this, that)
      case _ :TModType =>
        if(this == that) this else throw TypeMeetError(this, that)
      case _ :TLockedMemType =>
        if(this == that) this else throw TypeMeetError(this, that)
      case _ :TRequestHandle =>
        if(this == that) this else throw TypeMeetError(this, that)
      case TNamedType(name) =>
        that match {
          case TNamedType(name2) => if (name.v == name2.v) this else throw TypeMeetError(this, that)
          case _ => that
        }
      case TMaybe(btyp) => that match {
        case TMaybe(btyp2) => TMaybe(btyp ⊓ btyp2)
        case _ => throw TypeMeetError(this, that)
      }
      case width: TBitWidth => that match {
        case w2 :TBitWidth => (width, w2) match {
          case (TBitWidthLen(l1), TBitWidthLen(l2)) => TBitWidthLen(Math.max(l1, l2))
          case (w1, w2) if w1 ==== w2 => w1
          case _ => TBitWidthMax(width, w2)
        }
        case _ => throw TypeMeetError(this, that)
      }
      case _ => if (this.getClass == that.getClass) this else throw TypeMeetError(this, that)
    }

    /**
     * operator for type equality. basically sugar for Subtypes.areEqual
     * four equal signs instead of three so that JS users don't feel too welcome
     */
    def ====(that:Type) :Boolean = Subtypes.areEqual(this, that)

    /**
     * operator for type inequality
     */
    def =!=(that :Type) :Boolean = !(this ==== that)

    /**
     * operator to check subtypes. Again basically sugar
     */
    def <<=(that :Type) :Boolean = Subtypes.isSubtype(this, that)

    /**
     * operator to check supertypes. More sugar
     */
    def >>=(that :Type) :Boolean = Subtypes.isSubtype(that, this)

    /**
     * alias for ⊓ in case someone doesn't wanna type that
     */
    def meet(that :Type) :Type = ⊓(that)
  }
  // Types that can be upcast to Ints
  sealed trait IntType
  sealed trait TSignedNess extends Type
  {
    def signed() :Boolean = this match
    {
      case TSigned() => true
      case TUnsigned() => false
      case _ => false
    }
    def unsigned() :Boolean = this match {
      case TSigned() => false
      case TUnsigned() => true
      case _ => false
    }
  }
  object SignFactory
  {
    def ofBool(signed :Boolean) :TSignedNess =
      {
        if(signed) TSigned() else TUnsigned()
      }
  }
  case class TSigned() extends TSignedNess
  case class TUnsigned() extends TSignedNess
  case class TSignVar(id :Id) extends TSignedNess
  case class TSizedInt(len: TBitWidth, sign: TSignedNess) extends Type with IntType
  {
    override def setPos(newpos: Position): TSizedInt.this.type =
      {
        sign.setPos(newpos)
        super.setPos(newpos)
      }
  }
  case class TInteger() extends Type with IntType
  // Use case class instead of case object to get unique positions
  case class TString() extends Type
  case class TVoid() extends Type
  case class TBool() extends Type
  case class TFun(args: List[Type], ret: Type) extends Type
  {
    override def setPos(newpos: Position): TFun.this.type =
      {
        args.foreach(a => a.setPos(newpos))
        ret.setPos(newpos)
        super.setPos(newpos)
      }
  }
  case class TRecType(name: Id, fields: Map[Id, Type]) extends Type
  {
    override def setPos(newpos :Position) :TRecType.this.type =
      {
        fields.foreach(idtp => idtp._2.setPos(newpos))
        super.setPos(newpos)
      }
  }
  case class TMemType(elem: Type,
                      addrSize: Int,
                      readLatency: Latency = Latency.Asynchronous,
                      writeLatency: Latency = Latency.Asynchronous,
                      readPorts: Int,
                      writePorts: Int) extends Type
  case class TModType(inputs: List[Type], refs: List[Type], retType: Option[Type], name: Option[Id] = None) extends Type
  case class TLockedMemType(mem: TMemType, idSz: Option[Int], limpl: LockInterface) extends Type
  case class TReqHandle(tp :Type, rtyp :RequestType) extends Type
  //TODO merge these two together
  case class TRequestHandle(mod: Id, rtyp: RequestType) extends Type
  //This is primarily used for parsing and is basically just a type variable
  case class TNamedType(name: Id) extends Type
  case class TMaybe(btyp: Type) extends Type
  sealed trait TBitWidth extends Type
  {
    def getLen :Int = this.matchOrError(this.pos, "bit width", "bit width len")
    { case l : TBitWidthLen => l.len}

    def stringRep() :String
  }
  case class TBitWidthVar(name: Id) extends TBitWidth
  {
    override def stringRep(): String = name.v
  }
  object TBitWidthImplicits
  {
    implicit def fromIndex(idx :EIndex) :TBitWidth = idx match
    {
      case EIndConst(v) => TBitWidthLen(v)
      case EIndAdd(l, r) => TBitWidthAdd(fromIndex(l), fromIndex(r))
      case EIndSub(l, r) => TBitWidthSub(fromIndex(l), fromIndex(r))
      case EIndVar(id) => TBitWidthVar(id)
    }
    implicit def fromInt(i :Int) :TBitWidthLen = TBitWidthLen(i)
  }
  case class TBitWidthLen(len: Int) extends TBitWidth
  {
    override def stringRep(): String = len.toString
  }

  case class TBitWidthAdd(var b1: TBitWidth, var b2: TBitWidth) extends TBitWidth
  {
    if(b1.stringRep() < b2.stringRep())
      {
        println("SHOULDN'T BE HERE")
        val tmp = b1
        b1 = b2
        b2 = tmp
      }
    override def stringRep(): String =
      {
        val lst = (b1.stringRep() :: b2.stringRep() :: Nil).sorted
        val tmp = "A" + lst.head + "_" + lst(1) + "A"
        if (is_my_generic(lst.head, accept_lit = true) && is_my_generic(lst(1), accept_lit = true))
          generic_type_prefix + tmp
        else
          tmp
      }
  }
  object TBitWidthAdd
  {
    def apply(b1 :TBitWidth, b2 :TBitWidth) :TBitWidth =
    {
      (b1, b2) match {
        case (TBitWidthLen(l1), TBitWidthLen(l2)) => TBitWidthLen(l1 + l2)
        case _ if (b1.stringRep() < b2.stringRep()) => new TBitWidthAdd(b2, b1)
        case _ => new TBitWidthAdd(b1, b2)
      }
    }
  }
  case class TBitWidthSub(var b1: TBitWidth, var b2: TBitWidth) extends TBitWidth
    {
      override def stringRep(): String =
        {
          val tmp = "S" + b1.stringRep() + "_" + b2.stringRep() + "S"
          if (is_my_generic(b1, accept_lit = true) && is_my_generic(b2, accept_lit = true))
            generic_type_prefix + tmp
          else
            tmp
        }
    }
  object TBitWidthSub
    {
      def apply(b1 :TBitWidth, b2 :TBitWidth) :TBitWidth =
        {
          (b1, b2) match {
            case (TBitWidthLen(l1), TBitWidthLen(l2)) => TBitWidthLen(math.abs(l1 - l2))
            case _ => new TBitWidthSub(b1, b2)
          }
        }
    }



  case class TBitWidthMax(b1: TBitWidth, b2: TBitWidth) extends TBitWidth
  {
    override def stringRep(): String =
      {
        val lst = (b1.stringRep() :: b2.stringRep() :: Nil).sorted
        val tmp = "M" + lst.head + "_" + lst(1) + "M"
        if (is_my_generic(lst.head, accept_lit = true) && is_my_generic(lst(1), accept_lit = true))
          generic_type_prefix + tmp
        else
          tmp
      }

  }
  object TBitWidthMax
  {
    def apply(b1 :TBitWidth, b2 :TBitWidth) :TBitWidth =
      {
        (b1, b2) match {
          case (TBitWidthLen(l1), TBitWidthLen(l2)) => TBitWidthLen(math.max(l1, l2))
          case (TBitWidthVar(l1), TBitWidthVar(l2)) if l1 == l2 => b1
          case _ if b1.stringRep() < b2.stringRep() => new TBitWidthMax(b2, b1)
          case _ => new TBitWidthMax(b1, b2)
        }
      }
  }
  case class TObject(name: Id, typParams: List[Type], methods: Map[Id,(TFun, Latency)]) extends Type

  //returns false only if it represents an unlocked memory type
  def isLockedMemory(mem: Id): Boolean = mem.typ.get match { case _:TMemType => false; case _ => true }
  //returns false only if it represents an external (Verilog) module or a pipeline with no internal mems/submodules
  def isLockedModule(mod: Id): Boolean = mod.typ.get match {
    case TModType(_, refs, _, _) => refs.nonEmpty
    case TObject(_, _, _) => false
    case _ => true
  }
  def isSynchronousAccess(mem: Id, isWrite: Boolean): Boolean = mem.typ.get match {
    case TMemType(_, _, readLatency, writeLatency, _, _) =>
      val latency: Latency = if (isWrite) writeLatency else readLatency
      latency == Latency.Asynchronous
    case TLockedMemType(TMemType(_, _, readLatency, writeLatency, _, _), _, _) =>
      val latency: Latency = if (isWrite) writeLatency else readLatency
      latency == Latency.Asynchronous
    case _ => false
  }
  def getMemFromRequest(r: Type): Id = {
      r.matchOrError(r.pos, "Checkpoint Handle", "Checkpoint Request Type") {
        case TRequestHandle(mod, _) => mod
      }
  }

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
  def InvOp(): BitUOp = BitUOp("~")
  def MagOp(): NumUOp = NumUOp("abs")
  def SignOp(): NumUOp = NumUOp("signum")
  def AndOp(e1: Expr,e2: Expr): EBinop = EBinop(BoolOp("&&", OpConstructor.and), e1,e2)
  def OrOp(e1: Expr, e2: Expr): EBinop = EBinop(BoolOp("||", OpConstructor.or), e1, e2)
  def EqOp(e1: Expr, e2: Expr): EBinop = EBinop(EqOp("=="), e1, e2)

  sealed trait BOp extends Positional {
    val op: String;
    override def toString: String = this.op
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
    def isLVal: Boolean = this match {
      case _:EVar => true
      case _:EMemAccess => true
      case _ => false
    }
    def copyMeta(from: Expr): Expr = {
      setPos(from.pos)
      typ = from.typ
      portNum = from.portNum
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
  case class EMemAccess(mem: Id, index: Expr, wmask: Option[Expr] = None, inHandle: Option[EVar], outHandle: Option[EVar], isAtomic: Boolean) extends Expr with LockInfoAnnotation with HasCopyMeta
  {
    override val copyMeta: HasCopyMeta => EMemAccess =
      {
        case from :EMemAccess =>
        setPos(from.pos)
        portNum = from.portNum
        memOpType = from.memOpType
        granularity = from.granularity
        typ = from.typ
        this
      }
  }
  case class EBitExtract(num: Expr, start: EIndex, end :EIndex) extends Expr
  case class ETernary(cond: Expr, tval: Expr, fval: Expr) extends Expr
  case class EApp(func: Id, args: List[Expr]) extends Expr
  case class ECall(mod: Id, method: Option[Id] = None, args: List[Expr], isAtomic: Boolean) extends Expr
  case class EVar(id: Id) extends Expr
  case class ECast(ctyp: Type, exp: Expr) extends Expr
  {
    typ = Some(ctyp)
  }


  sealed trait EIndex extends Positional with TypeAnnotation
  case class EIndConst(v :Int) extends EIndex
  case class EIndAdd(l :EIndex, r :EIndex) extends EIndex
  case class EIndSub(l :EIndex, r :EIndex) extends EIndex
  case class EIndVar(id :Id) extends EIndex
  object EIndAdd
  {
    def apply(l :EIndex, r :EIndex) :EIndex = (l, r ) match {
      case (EIndConst(lc), EIndConst(rc)) => EIndConst(lc + rc)
      case _ => new EIndAdd(l, r)
    }
  }
  object EIndSub
    {
      def apply(l :EIndex, r :EIndex) :EIndex = (l, r ) match {
        case (EIndConst(lc), EIndConst(rc)) => EIndConst(math.abs(lc - rc))
        case _ => new EIndSub(l, r)
      }
    }

  sealed trait Command extends Positional with SMTPredicate with PortAnnotation with HasCopyMeta
  {
    override val copyMeta: HasCopyMeta => Command =
      {
        case from :Command =>
        setPos(from.pos)
        portNum = from.portNum
        predicateCtx = from.predicateCtx
        this
        case _ => this
      }
  }
  case class CSeq(c1: Command, c2: Command) extends Command
  case class CTBar(c1: Command, c2: Command) extends Command
  case class CIf(cond: Expr, cons: Command, alt: Command) extends Command
  case class CAssign(lhs: EVar, rhs: Expr) extends Command{
    if (!lhs.isLVal) throw UnexpectedLVal(lhs, "assignment")
  }
  case class CRecv(lhs: Expr, rhs: Expr) extends Command {
    if (!lhs.isLVal) throw UnexpectedLVal(lhs, "assignment")
  }
  case class CSpecCall(handle: EVar, pipe: Id, args: List[Expr]) extends Command
  case class CCheckSpec(isBlocking: Boolean) extends Command
  case class CVerify(handle: EVar, args: List[Expr], preds: List[EVar], update: Option[ECall], checkHandles: List[EVar]) extends Command
  case class CUpdate(newHandle: EVar, handle: EVar, args: List[Expr], preds: List[EVar], checkHandles: List[EVar]) extends Command
  case class CInvalidate(handle: EVar, checkHandles: List[EVar]) extends Command
  case class CPrint(args: List[Expr]) extends Command
  case class COutput(exp: Expr) extends Command
  case class CReturn(exp: Expr) extends Command
  case class CExpr(exp: Expr) extends Command
  case class CLockStart(mod: Id) extends Command
  case class CLockEnd(mod: Id) extends Command
  case class CCheckpoint(handle: EVar, lock: Id) extends Command
  case class CLockOp(mem: LockArg, op: LockState, var lockType: Option[LockType], args :List[Expr], ret :Option[EVar]) extends Command with LockInfoAnnotation
  {
    override val copyMeta: HasCopyMeta => CLockOp =
      {
        case from :CLockOp =>
          setPos(from.pos)
          portNum = from.portNum
          predicateCtx = from.predicateCtx
          lockType = from.lockType
          granularity = from.granularity
        this
      }
  }
  case class CSplit(cases: List[CaseObj], default: Command) extends Command
  case class CEmpty() extends Command


  sealed trait InternalCommand extends Command

  case class ICondCommand(cond: Expr, cs: List[Command]) extends InternalCommand
  case class IUpdate(specId: Id, value: EVar, originalSpec: EVar) extends InternalCommand
  case class ICheck(specId: Id, value: EVar) extends InternalCommand
  case class ISend(handle: EVar, receiver: Id, args: List[EVar]) extends InternalCommand
  case class IRecv(handle: EVar, sender: Id, result: EVar) extends InternalCommand
  //TODO Clean up what actually needs the lock info annotation
  case class IMemSend(memHandle: EVar, writeMask: Option[Expr], mem: Id,
                      data: Option[EVar], addr: EVar, lInHandle: Option[EVar], lOutHandle: Option[EVar],
                      isAtomic: Boolean) extends InternalCommand with LockInfoAnnotation {
    def isWrite: Boolean = data.isDefined
  }
  case class IMemRecv(mem: Id, handle: EVar, data: Option[EVar]) extends InternalCommand with LockInfoAnnotation
  //used for sequential memories that don't commit writes immediately but don't send a response
  case class IMemWrite(mem: Id, addr: EVar, data: EVar,
                       inHandle: Option[EVar], outHandle: Option[EVar], isAtomic: Boolean) extends InternalCommand with LockInfoAnnotation
  case class ICheckLockOwned(mem: LockArg, inHandle: EVar, outHandle :EVar) extends InternalCommand with LockInfoAnnotation
  case class IReserveLock(outHandle: EVar, mem: LockArg) extends InternalCommand with LockInfoAnnotation
  case class IAssignLock(handle: EVar, src: Expr, default: Option[Expr]) extends InternalCommand with LockInfoAnnotation
  case class IReleaseLock(mem: LockArg, inHandle: EVar) extends InternalCommand with LockInfoAnnotation
  //needed for internal compiler passes to track branches with explicitly no lockstate change
  case class ILockNoOp(mem: LockArg) extends InternalCommand

  case class CaseObj(cond: Expr, body: Command) extends Positional

  sealed trait Definition extends Positional

  case class FuncDef(
    name: Id,
    args: List[Param],
    ret: Type,
    body: Command,
    templateTypes :List[Id]) extends Definition with Provisos

  case class MethodDef(
                      name :Id,
                      args :List[Param],
                      ret :Type,
                      body :Command,
                      lat :Latency
                      ) extends Definition

  case class ModuleDef(
    name: Id,
    inputs: List[Param],
    modules: List[Param],
    ret: Option[Type],
    body: Command) extends Definition with RecursiveAnnotation with SpeculativeAnnotation with HasCopyMeta
    {
      override val copyMeta: HasCopyMeta => ModuleDef =
        {
          case from :ModuleDef =>
          maybeSpec = from.maybeSpec
          isRecursive = from.isRecursive
          pos = from.pos
          this
          case _ => this
        }
    }

  case class Param(name: Id, typ: Type) extends Positional

  case class ExternDef(name: Id, typParams: List[Type], methods: List[MethodDef]) extends Definition with TypeAnnotation

  case class Prog(exts: List[ExternDef],
    fdefs: List[FuncDef], moddefs: List[ModuleDef], circ: Circuit) extends Positional

  sealed trait Circuit extends Positional
  case class CirSeq(c1: Circuit, c2: Circuit) extends Circuit
  case class CirConnect(name: Id, c: CirExpr) extends Circuit
  case class CirExprStmt(ce: CirExpr) extends Circuit

  sealed trait CirExpr extends Expr
  case class CirMem(elemTyp: Type, addrSize: Int, numPorts: Int) extends CirExpr
  case class CirRegFile(elemTyp: Type, addrSize: Int) extends CirExpr
  case class CirRegister(elemTyp: Type, initVal: Int) extends CirExpr
  //TODO do these ever need other kinds of parameters besides ints?
  //this allows us to build a "locked" version of a memory
  case class CirLock(mem: Id, impl: LockInterface, szParams: List[Int]) extends CirExpr
  //This is an already "locked" memory (i.e. one line instantiation, no reference to the unlocked memory)
  case class CirLockMem(elemTyp: Type, addrSize: Int, impl: LockInterface, szParams: List[Int], numPorts: Int) extends CirExpr
  case class CirLockRegFile(elemTyp: Type, addrSize: Int, impl: LockInterface, szParams: List[Int]) extends CirExpr
  /* specialized is the list of ints which "specialize" the (potential) generics of mod */
  /* invariant: length of specialized matches up with # of generics in mod */
  case class CirNew(mod: Id, specialized :List[Int], mods: List[Id], params: List[EInt]) extends CirExpr
  case class CirCall(mod: Id, args: List[Expr]) extends CirExpr
}
