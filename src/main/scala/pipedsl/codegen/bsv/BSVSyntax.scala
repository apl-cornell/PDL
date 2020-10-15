package pipedsl.codegen.bsv

import pipedsl.common.Errors.{UnexpectedExpr, UnexpectedType}
import pipedsl.common.Syntax
import pipedsl.common.Syntax.Latency.Combinational
import pipedsl.common.Syntax.{BitOp, EApp, EBinop, EBitExtract, EBool, ECast, EInt, EMemAccess, ERecAccess, ERecLiteral, ETernary, EUop, EVar, Expr, Id, TBool, TFun, TMemType, TNamedType, TRecType, Type}

object BSVSyntax {

  sealed trait MethodType

  case object Action extends MethodType

  case class Value(rtyp: BSVType) extends MethodType

  case class ActionValue(rtyp: BSVType) extends MethodType

  sealed trait BSVType

  case class BCombMemType(elem: BSVType, addrSize: Int) extends BSVType

  case class BAsyncMemType(elem: BSVType, addrSize: Int) extends BSVType

  case class BStruct(name: String, fields: List[BVar]) extends BSVType

  case class BInterface(name: String, tparams: List[BVar] = List()) extends BSVType

  case class BSizedType(name: String, sizeParams: List[Integer] = List()) extends BSVType

  case class BSizedInt(unsigned: Boolean, size: Int) extends BSVType

  case class BTypeParam(name: String) extends BSVType

  case object BBool extends BSVType

  case object BVoid extends BSVType

  case object BEmptyModule extends BSVType

  class BSVTranslator(val modmap: Map[Id, BSVType] = Map(), val handleMap: Map[BSVType, BSVType] = Map()) {

    private var variablePrefix = ""

    def setVariablePrefix(p: String): Unit = variablePrefix = p

    def toBSVType(t: Type): BSVType = t match {
      case Syntax.TMemType(elem, addrSize, rlat, _) =>
        BluespecInterfaces.getMemType(isAsync = rlat != Combinational,
          BSizedInt(unsigned = true, addrSize), toBSVType(elem),
          Some(BluespecInterfaces.getDefaultMemHandleType))
      case Syntax.TSizedInt(len, unsigned) => BSizedInt(unsigned, len)
      case Syntax.TBool() => BBool
      case Syntax.TModType(_, _, _, Some(n)) => modmap(n)
      case Syntax.TModType(_, _, _, None) => throw UnexpectedType(t.pos, "Module type", "A Some(mod name) typ", t)
      case Syntax.TRequestHandle(n, isLock) =>
        if (isLock) {
          BluespecInterfaces.getDefaultLockHandleType
        } else {
          val modtyp = toBSVType(n.typ.get)
          if (handleMap.contains(modtyp)) {
            handleMap(modtyp)
          } else {
            //if not in the handle map, use the appropriate default handle size. If the
            //handle is for a normal module then there is no default
            n.typ.get match {
              case _: TMemType => BluespecInterfaces.getDefaultMemHandleType
              case _ => throw UnexpectedType(n.pos, "Module request handle", "A defined module req type", n.typ.get)
            }
          }
        }
      case Syntax.TVoid() => BVoid
      case TNamedType(name) => BTypeParam(name.v)
      //TODO implement function translation
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

    def toBSVExpr(e: Expr): BExpr = e match {
      case EInt(v, base, bits) => BIntLit(v, base, bits)
      case EBool(v) => BBoolLit(v)
      case EUop(op, ex) => BUOp(op.op, toBSVExpr(ex))
      case eb@EBinop(_, _, _) => toBSVBop(eb)
      case EBitExtract(num, start, end) => BUnpack(BBitExtract(BPack(toBSVExpr(num)), start, end))
      case ETernary(cond, tval, fval) => BTernaryExpr(toBSVExpr(cond), toBSVExpr(tval), toBSVExpr(fval))
      case e@EVar(_) => toBSVVar(e)
      //TODO functions
      //case EApp(func, args) => throw new UnexpectedExpr(e)
      case EApp(_, _) => BIntLit(0, 10, 1)
      case ERecAccess(_, _) => throw UnexpectedExpr(e)
      case ERecLiteral(_) => throw UnexpectedExpr(e)
      case EMemAccess(mem, index) => BluespecInterfaces.getCombRead(BVar(mem.v, toBSVType(mem.typ.get)), toBSVExpr(index))
      case ec@ECast(_, _) => translateCast(ec)
      case _ => throw UnexpectedExpr(e)
    }

    //TODO handle this better
    def translateCast(e: ECast): BExpr = {
      e.ctyp match {
        case TBool() => toBSVExpr(e.exp)
        case _ => throw UnexpectedType(e.pos, "Couldn't translate BSV cast",
          "TBool", e.ctyp)
      }
    }

    //TODO a better way to translate operators
    def toBSVBop(b: EBinop): BExpr = b.op match {
      case BitOp("++", _) => BUnpack(BConcat(BPack(toBSVExpr(b.e1)), List(BPack(toBSVExpr(b.e2)))))
      case _ => BBOp(b.op.op, toBSVExpr(b.e1), toBSVExpr(b.e2))
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

  case class BPack(e: BExpr) extends BExpr

  case class BUnpack(e: BExpr) extends BExpr

  case class BTernaryExpr(cond: BExpr, trueExpr: BExpr, falseExpr: BExpr) extends BExpr

  case class BBoolLit(v: Boolean) extends BExpr

  case class BIntLit(v: Int, base: Int, bits: Int) extends BExpr

  case class BStructLit(typ: BStruct, fields: Map[BVar, BExpr]) extends BExpr

  case class BStructAccess(rec: BExpr, field: BExpr) extends BExpr

  case class BVar(name: String, typ: BSVType) extends BExpr

  case class BBOp(op: String, lhs: BExpr, rhs: BExpr) extends BExpr

  case class BUOp(op: String, expr: BExpr) extends BExpr

  case class BBitExtract(expr: BExpr, start: Int, end: Int) extends BExpr

  case class BConcat(first: BExpr, rest: List[BExpr]) extends BExpr

  case class BModule(name: String, args: List[BExpr] = List()) extends BExpr

  case class BMethodInvoke(mod: BExpr, method: String, args: List[BExpr]) extends BExpr

  sealed trait BStatement

  case class BStmtSeq(stmts: List[BStatement]) extends BStatement

  case class BExprStmt(expr: BExpr) extends BStatement

  case class BReturnStmt(expr: BExpr) extends BStatement

  case class BModInst(lhs: BVar, rhs: BModule) extends BStatement

  case class BModAssign(lhs: BVar, rhs: BExpr) extends BStatement

  case class BAssign(lhs: BVar, rhs: BExpr) extends BStatement

  case class BInvokeAssign(lhs: BVar, rhs: BExpr) extends BStatement

  case class BDecl(lhs: BVar, rhs: Option[BExpr]) extends BStatement

  case class BIf(cond: BExpr, trueBranch: List[BStatement], falseBranch: List[BStatement]) extends BStatement

  case class BDisplay(fmt: String, args: List[BExpr]) extends BStatement

  case object BEmpty extends BStatement


  case class BStructDef(typ: BStruct, derives: List[String])

  case class BRuleDef(name: String, conds: List[BExpr], body: List[BStatement])

  case class BMethodSig(name: String, typ: MethodType, params: List[BVar])

  case class BFuncDef(name: String) //TODO the rest of this
  case class BMethodDef(sig: BMethodSig, cond: Option[BExpr] = None, body: List[BStatement])

  case class BModuleDef(name: String, typ: Option[BInterface],
    params: List[BVar], body: List[BStatement], rules: List[BRuleDef], methods: List[BMethodDef])

  case class BInterfaceDef(typ: BInterface, methods: List[BMethodSig])

  case class BImport(name: String)

  case class BExport(name: String, expFields: Boolean)

  case class BProgram(name: String, topModule: BModuleDef, imports: List[BImport], exports: List[BExport], structs: List[BStructDef],
    interfaces: List[BInterfaceDef], modules: List[BModuleDef])

}