package pipedsl.common

import pipedsl.common.Errors.UnexpectedExpr
import pipedsl.common.Syntax.Latency.{Combinational}
import pipedsl.common.Syntax._

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
  case class BSizedInt(unsigned: Boolean, size: Int) extends BSVType
  case object BBool extends BSVType
  case object BVoid extends BSVType
  case object BEmptyModule extends BSVType

  class BSVTranslator(var modmap: Map[Id, BSVType] = Map(), var handleMap: Map[Id, BSVType] = Map()) {

    private var variablePrefix = ""
    def setVariablePrefix(p: String): Unit = variablePrefix = p

    def toBSVType(t: Type): BSVType = t match {
      case Syntax.TMemType(elem, addrSize, rlat, _) if rlat == Combinational => BCombMemType(toBSVType(elem), addrSize)
      case Syntax.TMemType(elem, addrSize, _, _) => BAsyncMemType(toBSVType(elem), addrSize)
      case Syntax.TSizedInt(len, unsigned) => BSizedInt(unsigned, len)
      case Syntax.TBool() => BBool
      case Syntax.TModType(_, _, _, Some(n)) => modmap(n)
      case Syntax.TRequestHandle(n) => handleMap(n)
      case Syntax.TVoid() => BVoid
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
      case eb@EBinop(_,_,_) => toBSVBop(eb)
      case EBitExtract(num, start, end) => BBitExtract(toBSVExpr(num), start, end)
      case ETernary(cond, tval, fval) => BTernaryExpr(toBSVExpr(cond), toBSVExpr(tval), toBSVExpr(fval))
      case e@EVar(id) => toBSVVar(e)
      //TODO functions
      //case EApp(func, args) => throw new UnexpectedExpr(e)
      case EApp(func, args) => BIntLit(0, 10, 1)
      case ERecAccess(rec, fieldName) => throw new UnexpectedExpr(e)
      case ERecLiteral(fields) => throw new UnexpectedExpr(e)
      case EMemAccess(mem, index) => BMemRead(BVar(mem.v, toBSVType(mem.typ.get)), toBSVExpr(index))
      case ECast(ctyp, exp) => throw new UnexpectedExpr(e)
      case expr: CirExpr =>throw new UnexpectedExpr(e)
    }

    //TODO a better way to translate operators
    def toBSVBop(b: EBinop): BExpr = b.op match {
      case BitOp("++", _) => BConcat(toBSVExpr(b.e1), List(toBSVExpr(b.e2)))
      case _ => BBOp(b.op.op, toBSVExpr(b.e1), toBSVExpr(b.e2))
    }
  }



  /**
   *
   * @param typ
   * @return
   */
  def getCanonicalStruct(typ: BStruct, t: BSVTranslator): BStructLit = {
    val argmap = typ.fields.foldLeft(Map[BVar, BExpr]())((m, f) => {
      //don't translate field name, do translate RHS
      m + (BVar(f.name, f.typ) -> t.toBSVVar(f))
    })
    BStructLit(typ, argmap)
  }

  /**
   *
   * @param typ
   * @param args
   * @return
   */
  def getNamedStruct(typ: BStruct, args: Iterable[BExpr]): BStructLit = {
    BStructLit(typ, typ.fields.zip(args).toMap)
  }

  sealed trait BExpr

  case object BDontCare extends BExpr
  case object BZero extends BExpr
  case object BOne extends BExpr
  case object BTime extends BExpr
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
  case class BMemRead(mem: BVar, addr: BExpr) extends BExpr
  case class BMemPeek(mem: BVar) extends BExpr


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
  case class BMemReadReq(mem: BVar, addr: BExpr) extends BStatement
  case class BMemReadResp(lhs: BVar, mem: BVar) extends BStatement
  case class BMemWrite(mem: BVar, addr: BExpr, data: BExpr) extends BStatement
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

  /**
   *
   * @param name
   * @param rules
   * @return
   */
  def combineRules(name: String, rules: Iterable[BRuleDef]): Option[BRuleDef] = {
      val newcond = rules.foldLeft(List[BExpr]())((l, r) => {
        l ++ r.conds
      })
      val newstmts = rules.foldLeft(List[BStatement]())((l, r) => {
        l ++ r.body
      })
      if (newstmts.isEmpty) {
        None
      } else {
        Some(BRuleDef(name, newcond, newstmts))
      }
  }

  /**
   *
   * @param rule
   * @param stmts
   * @return
   */
  def addStmts(rule: BRuleDef, stmts: Iterable[BStatement]): BRuleDef = {
    BRuleDef(rule.name, rule.conds, rule.body ++ stmts)
  }
}
