package pipedsl.codegen.bsv

import pipedsl.codegen.Translations.Translator
import pipedsl.common.Errors.{UnexpectedBSVType, UnexpectedCommand, UnexpectedExpr, UnexpectedType}
import pipedsl.common.LockImplementation.LockInterface
import pipedsl.common.Syntax.Latency.Combinational
import pipedsl.common.Syntax._

object BSVSyntax {

  sealed trait MethodType
  case object Action extends MethodType
  case class Value(rtyp: BSVType) extends MethodType
  case class ActionValue(rtyp: BSVType) extends MethodType

  sealed trait Proviso
  case class PBits(szName: String) extends Proviso

  sealed trait BSVType
  case class BNumericType(sz: Int) extends BSVType
  case class BCombMemType(elem: BSVType, addrSize: Int) extends BSVType
  case class BAsyncMemType(elem: BSVType, addrSize: Int) extends BSVType
  case class BStruct(name: String, fields: List[BVar]) extends BSVType
  case class BInterface(name: String, tparams: List[BVar] = List()) extends BSVType
  case class BSizedType(name: String, sizeParams: List[Integer] = List()) extends BSVType
  case class BSizedInt(unsigned: Boolean, size: Int) extends BSVType
  case class BTypeParam(name: String, provisos: List[Proviso]) extends BSVType
  case object BBool extends BSVType
  case object BString extends BSVType
  case object BVoid extends BSVType
  case object BEmptyModule extends BSVType

  def BMaybe(t: BSVType): BInterface = BInterface("Maybe", List(BVar("basetype", t)))

  class BSVTranslator(val bsints: BluespecInterfaces, val modmap: Map[Id, BSVType] = Map(),
    val handleMap: Map[BSVType, BSVType] = Map()) extends Translator[BSVType, BExpr, BVar, BFuncDef] {

    private var variablePrefix = ""

    def setVariablePrefix(p: String): Unit = variablePrefix = p

    //TODO it would be nice not to need two type translation methods
    def toTypeForMod(t: Type, n: Id): BSVType = t match {
      case TLockedMemType(mem, idsz, limpl) =>
        val lidtyp = if (idsz.isDefined) BSizedInt(unsigned = true, idsz.get) else modmap(n)
        val mtyp = bsints.getBaseMemType(isAsync = mem.readLatency != Combinational,
          BSizedInt(unsigned = true, mem.addrSize), toType(mem.elem))
        getLockedMemType(mem, mtyp, lidtyp, limpl, useTypeVars = true, Some(n))
      case _ => toType(t)
    }

    def toType(t: Type): BSVType = t match {
      case TMemType(elem, addrSize, rlat, _) =>
        bsints.getBaseMemType(isAsync = rlat != Combinational,
          BSizedInt(unsigned = true, addrSize), toType(elem))
      case TLockedMemType(mem, idsz, limpl) =>
        val mtyp = toType(mem).matchOrError() { case c: BInterface => c }
        val lidtyp =  if (limpl.useUniqueLockId()) {
          if (idsz.isDefined) {
            bsints.getLockHandleType(idsz.get)
          } else bsints.getDefaultLockHandleType
        } else {
          //re-use the id from the memory
          mtyp.tparams.last.typ
        }
        getLockedMemType(mem, mtyp, lidtyp, limpl, useTypeVars = false, None)
      case TSizedInt(len, unsigned) => BSizedInt(unsigned, len)
      case TBool() => BBool
      case TString() => BString
      case TModType(_, _, _, Some(n)) => modmap(n)
      case TModType(_, _, _, None) => throw UnexpectedType(t.pos, "Module type", "A Some(mod name) typ", t)
      case TMaybe(btyp) => BMaybe(toType(btyp))
      case TRequestHandle(n, rtyp) => rtyp match {
        case pipedsl.common.Syntax.RequestType.Lock =>
          //These are passed in the modmap rather than the handle map
          modmap(n)
        case pipedsl.common.Syntax.RequestType.Module =>
          val modtyp = toType(n.typ.get)
          if (handleMap.contains(modtyp)) {
            handleMap(modtyp)
          } else {
            //if not in the handle map, use the appropriate default handle size. If the
            //handle is for a normal module then there is no default
            n.typ.get match {
              case l: TLockedMemType => if(l.limpl.useUniqueLockId()) bsints.getDefaultMemHandleType else modmap(n)
              case _: TMemType => bsints.getDefaultMemHandleType
              case _ => throw UnexpectedType(n.pos, "Module request handle", "A defined module req type", n.typ.get)
            }
          }
          //TODO allow this to be specified somewhere
        case pipedsl.common.Syntax.RequestType.Speculation => bsints.getDefaultSpecHandleType
      }
      case TVoid() => BVoid
      case TNamedType(name) => BTypeParam(name.v, List(PBits("_sz" + name.v)))
      //TODO implement function type translation
      case TFun(_, _) => throw new RuntimeException
      //TODO better error
      case TRecType(_, _) => throw new RuntimeException
    }

    def toBSVVar(v: BVar): BVar = {
      BVar(variablePrefix + v.name, v.typ)
    }

    def toVar(i: Id): BVar = {
      BVar(variablePrefix + i.v, toType(i.typ.get))
    }

    def toVar(v: EVar): BVar = {
      toVar(v.id)
    }

    def toExpr(e: Expr): BExpr = e match {
      case EInt(v, base, bits) => BIntLit(v, base, bits)
      case EBool(v) => BBoolLit(v)
      case EString(v) => BStringLit(v)
      case EUop(op, ex) => BUOp(op.op, toExpr(ex))
      case eb@EBinop(_, _, _) => toBSVBop(eb)
      case EBitExtract(num, start, end) =>
          val bnum = toExpr(num)
          //remove nested pack/unpacks
          bnum match {
            case BUnpack(e) => BUnpack(BBitExtract(e, start, end))
            case e => BUnpack(BBitExtract(BPack(e), start, end))
          }
      case ETernary(cond, tval, fval) => BTernaryExpr(toExpr(cond), toExpr(tval), toExpr(fval))
      case e@EVar(_) => toVar(e)
      case EApp(func, args) => BFuncCall(func.v, args.map(a => toExpr(a)))
      case ERecAccess(_, _) => throw UnexpectedExpr(e)
      case ERecLiteral(_) => throw UnexpectedExpr(e)
      case EMemAccess(mem, index) =>
        bsints.getCombRead(BVar(mem.v, toType(mem.typ.get)), toExpr(index))
      case ec@ECast(_, _) => translateCast(ec)
      case EIsValid(ex) => BIsValid(toExpr(ex))
      case EInvalid => BInvalid
      case EFromMaybe(ex) => BFromMaybe(BDontCare, toExpr(ex))
      case EToMaybe(ex) => BTaggedValid(toExpr(ex))
      case _ => throw UnexpectedExpr(e)
    }

    //TODO handle casts better
    private def translateCast(e: ECast): BExpr = {
      e.ctyp match {
        case TBool() => toExpr(e.exp)
        case _ => throw UnexpectedType(e.pos, "Couldn't translate BSV cast",
          "TBool", e.ctyp)
      }
    }

    //TODO a better way to translate operators
    private def toBSVBop(b: EBinop): BExpr = b.op match {
      case BitOp("++", _) =>
        val left = toExpr(b.e1) match {
          case BUnpack(e) => e
          case e => BPack(e)
        }
        val right = toExpr(b.e2) match {
          case BUnpack(e) => e
          case e => BPack(e)
        }
        BUnpack(BConcat(left, List(right)))
      case _ => BBOp(b.op.op, toExpr(b.e1), toExpr(b.e2))
    }

    private def getLockedMemType(m: TMemType, mtyp: BInterface, lockIdTyp: BSVType,
      limpl: LockInterface, useTypeVars:Boolean = false, paramId: Option[Id]): BInterface = {
      val intName = limpl.getModuleName(m)
      val lparams = limpl.getTypeArgs(List()).zipWithIndex.map(a => {
        val sz = a._1
        val idx = a._2
        if (useTypeVars) {
          //Don't use the Bits#() proviso for this type since it is a static integer not a wire type
          BVar("_unused_", BTypeParam("_szParam_" + idx + "_" + paramId.get, List()))
        } else {
          BVar("_unused_", BNumericType(sz))
       }})
      //replace last tparam w/ lockidtyp if not using a unique id
      val tmpparams = if (limpl.useUniqueLockId()) { mtyp.tparams } else { mtyp.tparams.init }
      val params = (tmpparams :+ BVar("lidtyp", lockIdTyp)) ++ lparams
      BInterface(intName, params)
    }

    //This updates the translator's map of already defined functions
    //so that they can be used by later translation operations
    def toFunc(b: FuncDef): BFuncDef = {
      val rettype = toType(b.ret)
      val params = b.args.foldLeft(List[BVar]())((ps, arg) => {
        ps :+ BVar(arg.name.v, toType(arg.typ))
      })
      val fdef = BFuncDef(b.name.v, rettype, params, translateFuncBody(b.body))
      fdef
    }

    private def translateFuncBody(c: Command): List[BStatement] = c match {
      case CSeq(c1, c2) =>
        translateFuncBody(c1) ++ translateFuncBody(c2)
      case CIf(cond, cons, alt) =>
        List(BIf(toExpr(cond), translateFuncBody(cons), translateFuncBody(alt)))
      case CAssign(lhs, rhs) =>
        List(BDecl(toVar(lhs), Some(toExpr(rhs))))
      case CReturn(exp) =>
        List(BReturnStmt(toExpr(exp)))
      case CExpr(exp) =>
        List(BExprStmt(toExpr(exp)))
      //TODO case Syntax.CSplit(cases, default) =>
      case CEmpty() => List()
      case _ => throw UnexpectedCommand(c)
    }
  }


  /**
   * This creates a struct literal from the given struct type
   * and translator. This assumes that the arguments to struct creation
   * are variables with the same name as the fields (modulo any variable renaming
   * that translator may do).
   *
   * @param typ The BSV Struct Type
   * @param t   The translator object that will potentially rename the constructor arguments
   * @return The canonical struct literal for type typ.
   */
  def getCanonicalStruct(typ: BStruct, t: BSVTranslator): BStructLit = {
    val argmap = typ.fields.foldLeft(Map[BVar, BExpr]())((m, f) => {
      //don't translate field name, do translate RHS
      m + (BVar(f.name, f.typ) -> t.toBSVVar(f))
    })
    BStructLit(typ, argmap)
  }

  /**
   * This creates a struct literal from the given struct type
   * and specified arguments. This assumes that enough arguments
   * have been provided to set each field of the struct and that they
   * have been provided in the appropriate order (same as the order
   * of the fields in typ.
   *
   * @param typ  The BSV Struct Type
   * @param args The arguments to construct the struct with.
   * @return A Struct Literal created from args of type typ.
   */
  def getNamedStruct(typ: BStruct, args: Iterable[BExpr]): BStructLit = {
    BStructLit(typ, typ.fields.zip(args).toMap)
  }

  sealed trait BExpr

  case object BDontCare extends BExpr
  case object BZero extends BExpr
  case object BOne extends BExpr
  case object BTime extends BExpr
  case object BInvalid extends BExpr
  case class BTaggedValid(exp: BExpr) extends BExpr
  case class BFromMaybe(default: BExpr, exp: BExpr) extends BExpr
  case class BIsValid(exp: BExpr) extends BExpr
  case class BPack(e: BExpr) extends BExpr
  case class BUnpack(e: BExpr) extends BExpr
  case class BTernaryExpr(cond: BExpr, trueExpr: BExpr, falseExpr: BExpr) extends BExpr
  case class BBoolLit(v: Boolean) extends BExpr
  case class BUnsizedInt(v: Int) extends BExpr
  case class BIntLit(v: Int, base: Int, bits: Int) extends BExpr
  case class BStringLit(v: String) extends BExpr
  case class BStructLit(typ: BStruct, fields: Map[BVar, BExpr]) extends BExpr
  case class BStructAccess(rec: BExpr, field: BExpr) extends BExpr
  case class BVar(name: String, typ: BSVType) extends BExpr
  case class BBOp(op: String, lhs: BExpr, rhs: BExpr) extends BExpr
  case class BUOp(op: String, expr: BExpr) extends BExpr
  case class BBitExtract(expr: BExpr, start: Int, end: Int) extends BExpr
  case class BConcat(first: BExpr, rest: List[BExpr]) extends BExpr
  case class BModule(name: String, args: List[BExpr] = List()) extends BExpr
  case class BMethodInvoke(mod: BExpr, method: String, args: List[BExpr]) extends BExpr
  case class BFuncCall(func: String, args: List[BExpr]) extends BExpr

  sealed trait BStatement {
    var useLet: Boolean = false
    def setUseLet(b: Boolean): BStatement = { this.useLet = b; this }
  }

  case class BStmtSeq(stmts: List[BStatement]) extends BStatement

  case class BExprStmt(expr: BExpr) extends BStatement

  case class BReturnStmt(expr: BExpr) extends BStatement

  case class BModInst(lhs: BVar, rhs: BModule) extends BStatement

  case class BModAssign(lhs: BVar, rhs: BExpr) extends BStatement

  case class BAssign(lhs: BVar, rhs: BExpr) extends BStatement

  case class BInvokeAssign(lhs: BVar, rhs: BExpr) extends BStatement

  case class BIntAssign(lhs: BVar, rhs: BVar) extends BStatement

  case class BDecl(lhs: BVar, rhs: Option[BExpr]) extends BStatement

  case class BIf(cond: BExpr, trueBranch: List[BStatement], falseBranch: List[BStatement]) extends BStatement

  case class BDisplay(fmt: String, args: List[BExpr]) extends BStatement

  case object BFinish extends BStatement
  
  case class BDisplayVar(BVar: BVar) extends BStatement
  
  case object BEmpty extends BStatement

  case class BStructDef(typ: BStruct, derives: List[String])

  case class BRuleDef(name: String, conds: List[BExpr], body: List[BStatement])

  case class BMethodSig(name: String, typ: MethodType, params: List[BVar])

  case class BFuncDef(name: String, rettyp: BSVType, params: List[BVar], body: List[BStatement])

  case class BMethodDef(sig: BMethodSig, cond: Option[BExpr] = None, body: List[BStatement])


  case class BModuleDef(name: String, typ: Option[BInterface],
    params: List[BVar], body: List[BStatement], rules: List[BRuleDef], methods: List[BMethodDef])

  case class BInterfaceDef(typ: BInterface, methods: List[BMethodSig], subints: List[BVar] = List())

  case class BImport(name: String)

  case class BExport(name: String, expFields: Boolean)

  case class BProgram(name: String, topModule: BModuleDef, imports: List[BImport], exports: List[BExport], structs: List[BStructDef],
    interfaces: List[BInterfaceDef], modules: List[BModuleDef])


  /**
   * Define common helper methods implicit classes.
   */
  implicit class RichType(typ: BSVType) {
    def matchOrError[A]()
      (andThen: PartialFunction[BSVType, A]): A = {
      val mismatchError: PartialFunction[BSVType, A] = {
        case _ => throw UnexpectedBSVType(s"Didn't expect BSV Type $typ")
      }
      andThen.orElse(mismatchError)(typ)
    }
  }
}
