package pipedsl.codegen.bsv

import pipedsl.common.Errors.{UnexpectedBSVType, UnexpectedCommand, UnexpectedExpr, UnexpectedType}
import pipedsl.common.Syntax.Latency.Combinational
import pipedsl.common.Syntax._

object BSVSyntax {

  sealed trait MethodType
  case object Action extends MethodType
  case class Value(rtyp: BSVType) extends MethodType
  case class ActionValue(rtyp: BSVType) extends MethodType

  sealed trait BSVType
  case class BNumericType(sz: Int) extends BSVType
  case class BCombMemType(elem: BSVType, addrSize: Int) extends BSVType
  case class BAsyncMemType(elem: BSVType, addrSize: Int) extends BSVType
  case class BStruct(name: String, fields: List[BVar]) extends BSVType
  case class BInterface(name: String, tparams: List[BVar] = List()) extends BSVType
  case class BSizedType(name: String, sizeParams: List[Integer] = List()) extends BSVType
  case class BSizedInt(unsigned: Boolean, size: Int) extends BSVType
  case class BTypeParam(name: String) extends BSVType
  case object BBool extends BSVType
  case object BString extends BSVType
  case object BVoid extends BSVType
  case object BEmptyModule extends BSVType

  class BSVTranslator(val bsints: BluespecInterfaces,
    val modmap: Map[Id, BSVType] = Map(), val handleMap: Map[BSVType, BSVType] = Map()) {

    private var variablePrefix = ""

    def setVariablePrefix(p: String): Unit = variablePrefix = p

    def toBSVType(t: Type): BSVType = t match {
        //TODO incorporate lock name
      case TMemType(elem, addrSize, rlat, _, limpl) =>
        bsints.getMemType(isAsync = rlat != Combinational,
          BSizedInt(unsigned = true, addrSize), toBSVType(elem), limpl)
      case TSizedInt(len, unsigned) => BSizedInt(unsigned, len)
      case TBool() => BBool
      case TString() => BString
      case TModType(_, _, _, Some(n)) => modmap(n)
      case TModType(_, _, _, None) => throw UnexpectedType(t.pos, "Module type", "A Some(mod name) typ", t)
      case TRequestHandle(n, isLock) =>
        if (isLock) {
          BInterface("Maybe", List(BVar("basehandletyp", bsints.getDefaultLockHandleType)))
        } else {
          val modtyp = toBSVType(n.typ.get)
          if (handleMap.contains(modtyp)) {
            handleMap(modtyp)
          } else {
            //if not in the handle map, use the appropriate default handle size. If the
            //handle is for a normal module then there is no default
            n.typ.get match {
              case _: TMemType => bsints.getDefaultMemHandleType
              case _ => throw UnexpectedType(n.pos, "Module request handle", "A defined module req type", n.typ.get)
            }
          }
        }
      case TVoid() => BVoid
      case TNamedType(name) => BTypeParam(name.v)
      //TODO implement function type translation
      case TFun(_, _) => throw new RuntimeException
      //TODO better error
      case TRecType(_, _) => throw new RuntimeException
    }

    def toBSVVar(v: BVar): BVar = {
      BVar(variablePrefix + v.name, v.typ)
    }

    def toBSVVar(i: Id): BVar = {
      BVar(variablePrefix + i.v, toBSVType(i.typ.get))
    }

    def toBSVVar(v: EVar): BVar = {
      toBSVVar(v.id)
    }

    def toBSVVar(v: Option[EVar]): Option[BVar] = v match {
      case Some(value) => Some(toBSVVar(value))
      case None => None
    }

    def toBSVExpr(e: Option[Expr]): Option[BExpr] = e match {
      case Some(value) => Some(toBSVExpr(value))
      case None => None
    }
    def toBSVExpr(e: Expr): BExpr = e match {
      case EInt(v, base, bits) => BIntLit(v, base, bits)
      case EBool(v) => BBoolLit(v)
      case EString(v) => BStringLit(v)
      case EUop(op, ex) => BUOp(op.op, toBSVExpr(ex))
      case eb@EBinop(_, _, _) => toBSVBop(eb)
      case EBitExtract(num, start, end) =>
          val bnum = toBSVExpr(num)
        //remove nested pack/unpacks
          bnum match {
            case BUnpack(e) => BUnpack(BBitExtract(e, start, end))
            case e => BUnpack(BBitExtract(BPack(e), start, end))
          }
      case ETernary(cond, tval, fval) => BTernaryExpr(toBSVExpr(cond), toBSVExpr(tval), toBSVExpr(fval))
      case e@EVar(_) => toBSVVar(e)
      case EApp(func, args) => BFuncCall(func.v, args.map(a => toBSVExpr(a)))
      case ERecAccess(_, _) => throw UnexpectedExpr(e)
      case ERecLiteral(_) => throw UnexpectedExpr(e)
      case EMemAccess(mem, index) => bsints.getCombRead(BVar(mem.v, toBSVType(mem.typ.get)), toBSVExpr(index))
      case ec@ECast(_, _) => translateCast(ec)
      case EIsValid(ex) => BIsValid(toBSVExpr(ex))
      case EInvalid => BInvalid
      case EFromMaybe(ex) => BFromMaybe(BDontCare, toBSVExpr(ex))
      case _ => throw UnexpectedExpr(e)
    }

    //TODO handle casts better
    def translateCast(e: ECast): BExpr = {
      e.ctyp match {
        case TBool() => toBSVExpr(e.exp)
        case _ => throw UnexpectedType(e.pos, "Couldn't translate BSV cast",
          "TBool", e.ctyp)
      }
    }

    //TODO a better way to translate operators
    def toBSVBop(b: EBinop): BExpr = b.op match {
      case BitOp("++", _) =>
        val left = toBSVExpr(b.e1) match {
          case BUnpack(e) => e
          case e => BPack(e)
        }
        val right = toBSVExpr(b.e2) match {
          case BUnpack(e) => e
          case e => BPack(e)
        }
        BUnpack(BConcat(left, List(right)))
      case _ => BBOp(b.op.op, toBSVExpr(b.e1), toBSVExpr(b.e2))
    }

    //This updates the translator's map of already defined functions
    //so that they can be used by later translation operations
    def toBSVFunc(b: FuncDef): BFuncDef = {
      val rettype = toBSVType(b.ret)
      val params = b.args.foldLeft(List[BVar]())((ps, arg) => {
        ps :+ BVar(arg.name.v, toBSVType(arg.typ))
      })
      val fdef = BFuncDef(b.name.v, rettype, params, translateFuncBody(b.body))
      fdef
    }

    private def translateFuncBody(c: Command): List[BStatement] = c match {
      case CSeq(c1, c2) =>
        translateFuncBody(c1) ++ translateFuncBody(c2)
      case CIf(cond, cons, alt) =>
        List(BIf(toBSVExpr(cond), translateFuncBody(cons), translateFuncBody(alt)))
      case CAssign(lhs, rhs) =>
        List(BDecl(toBSVVar(lhs), Some(toBSVExpr(rhs))))
      case CReturn(exp) =>
        List(BReturnStmt(toBSVExpr(exp)))
      case CExpr(exp) =>
        List(BExprStmt(toBSVExpr(exp)))
      //TODO case Syntax.CSplit(cases, default) =>
      case CEmpty => List()
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
  case class BFromMaybe(default: BExpr, exp: BExpr) extends BExpr
  case class BIsValid(exp: BExpr) extends BExpr
  case class BPack(e: BExpr) extends BExpr
  case class BUnpack(e: BExpr) extends BExpr
  case class BTernaryExpr(cond: BExpr, trueExpr: BExpr, falseExpr: BExpr) extends BExpr
  case class BBoolLit(v: Boolean) extends BExpr
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

  sealed trait BStatement

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
