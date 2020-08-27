package pipedsl.common

import pipedsl.common.BSVSyntax.MethodType.MethodType
import pipedsl.common.Errors.UnexpectedExpr
import pipedsl.common.Syntax._

object BSVSyntax {

  object MethodType extends Enumeration {
    type MethodType = Value
    val Action, Val, ActionVal = Value
  }

  sealed trait BSVType


  case class BMemType(elem: BSVType, addrSize: Int) extends BSVType
  case class BStruct(name: String, fields: List[BVar]) extends BSVType
  case class BInterface(name: String, tparams: List[BVar] = List()) extends BSVType
  case class BSizedInt(unsigned: Boolean, size: Int) extends BSVType
  case object BBool extends BSVType
  case object BEmptyModule extends BSVType

  /**
   *
   * @param t
   * @return
   */
  def toBSVType(t: Type): BSVType = t match {
    case Syntax.TMemType(elem, addrSize) => BMemType(toBSVType(elem), addrSize)
    case Syntax.TSizedInt(len, unsigned) => BSizedInt(unsigned, len)
    case Syntax.TBool() => BBool
      //TODO others
  }

  //TODO a better way to translate operators
  def toBSVBop(b: EBinop): BExpr = b.op match {
    case BitOp("++", _) => BConcat(toBSVExpr(b.e1), List(toBSVExpr(b.e2)))
    case _ => BBOp(b.op.op, toBSVExpr(b.e1), toBSVExpr(b.e2))
  }

  /**
   *
   * @param e
   * @return
   */
  def toBSVExpr(e: Expr): BExpr = e match {
    case EInt(v, base, bits) => BIntLit(v, base, bits)
    case EBool(v) => BBoolLit(v)
    case EUop(op, ex) => BUOp(op.op, toBSVExpr(ex))
    case eb@EBinop(_,_,_) => toBSVBop(eb)
    case EBitExtract(num, start, end) => BBitExtract(toBSVExpr(num), start, end)
    case ETernary(cond, tval, fval) => BCaseExpr(toBSVExpr(cond),
      List((BBoolLit(true), toBSVExpr(tval)),
        (BBoolLit(false), toBSVExpr(fval))))
    case EVar(id) => BVar(id.v, toBSVType(id.typ.get))
      //TODO functions
    //case EApp(func, args) => throw new UnexpectedExpr(e)
    case EApp(func, args) => BIntLit(0, 10, 1)
    case ERecAccess(rec, fieldName) => throw new UnexpectedExpr(e)
    case ERecLiteral(fields) => throw new UnexpectedExpr(e)
    case EMemAccess(mem, index) => throw new UnexpectedExpr(e)
    case ECast(ctyp, exp) => throw new UnexpectedExpr(e)
    case expr: CirExpr =>throw new UnexpectedExpr(e)
  }

  /**
   *
   * @param typ
   * @return
   */
  def getCanonicalStruct(typ: BStruct): BStructLit = {
    val argmap = typ.fields.foldLeft(Map[BVar, BExpr]())((m, f) => {
      m + (BVar(f.name, f.typ) -> BVar(f.name, f.typ))
    })
    BStructLit(typ, argmap)
  }

  sealed trait BExpr

  case object BDontCare extends BExpr
  case class BCaseExpr(cond: BExpr, cases: List[(BExpr, BExpr)]) extends BExpr
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
  case class BModInst(lhs: BVar, rhs: BModule) extends BStatement
  case class BModAssign(lhs: BVar, rhs: BExpr) extends BStatement
  case class BAssign(lhs: BVar, rhs: BExpr) extends BStatement
  case class BInvokeAssign(lhs: BVar, rhs: BExpr) extends BStatement
  case class BDecl(lhs: BVar, rhs: BExpr) extends BStatement
  case class BIf(cond: BExpr, trueBranch: List[BStatement], falseBranch: List[BStatement]) extends BStatement


  case class BStructDef(typ: BStruct, derives: List[String])
  case class BRuleDef(name: String, conds: List[BExpr], body: List[BStatement])
  case class BMethodSig(name: String, typ: MethodType, params: List[BVar])
  case class BMethodDef(sig: BMethodSig, body: List[BStatement])
  case class BModuleDef(name: String, typ: Option[BInterface],
    params: List[BVar], body: List[BStatement], rules: List[BRuleDef], methods: List[BMethodDef])
  case class BInterfaceDef(typ: BInterface, methods: List[BMethodSig])
  case class BImport(name: String)
  case class BProgram(topModule: BModuleDef, imports: List[BImport], structs: List[BStructDef],
    interfaces: List[BInterfaceDef], modules: List[BModuleDef])

  /**
   *
   * @param name
   * @param rules
   * @return
   */
  def combineRules(name: String, rules: Iterable[BRuleDef]): BRuleDef = {
    val newcond = rules.foldLeft(List[BExpr]())((l, r) => {
      l ++ r.conds
    })
    val newstmts = rules.foldLeft(List[BStatement]())((l, r) => {
      l ++ r.body
    })
    BRuleDef(name, newcond, newstmts)
  }
}
