package pipedsl.common

import pipedsl.common.Syntax._

object BSVPrettyPrinter {

  def printBSVType(t: Type): String = t match {
    case TSizedInt(len, unsigned) => (if (unsigned) "U" else "") + "Int#(" + len.toString + ")"
    case TVoid() => "Void"
    case TBool() => "Bool"
    case TFun(args, ret) => "TODO FUNC"
    case TRecType(name, fields) => "TODO REC TYPE"
    case TMemType(elem, addrSize) => "TODO MEM TYPE"
    case TModType(_, _) => "TODO MOD TYPE"
  }

  def printBSVCommand(c: Command, indent: String = ""): String = indent +
    (c match {
    case CSeq(c1, c2) => "SEQUENCE NOT EXPECTED"
    case CTBar(c1, c2) => "SEQUENCE NOT EXPECTED"
    case CIf(cond, cons, alt) => "IF NOT EXPECTED"
    case CAssign(lhs, rhs) => printBSVType(lhs.typ.get) + " " +
      printBSVExpr(lhs, indent) + " = " + printBSVExpr(rhs, indent);
    case CRecv(lhs, rhs) => "RECV NOT EXPECTED"
    case CCall(id, args) => "CALL NOT IMPLEMENTED"
    case COutput(exp) => "OUTPUT NOT IMPLEMENTED"
    case CReturn(exp) => "RETURN NOT IMPLEMENTED"
    case CExpr(exp) => printBSVExpr(exp, indent)
    case CLockOp(mem, op) => "LOCK NOT IMPLEMENTED"
    case CSpeculate(predVar, predVal, verify, body) => "SPECULTED NOT EXPECTED"
    case CCheck(predVar) => "CCHECK NOT EXPECTED"
    case CSplit(cases, default) => "CSPLIT NOT EXPECTED"
    case Syntax.CEmpty => "\n"
    case _ => "TODO / Uncompilable command"
  })

  def printBSVExpr(e: Expr, indent: String = ""): String = e match {
    case EInt(v, base, bits) => "TODO"
    case EBool(v) => if (v) { "True" } else { "False" }
    case EUop(op, ex) => "(" + op + printBSVExpr(ex) + ")"
    case EBinop(op, e1, e2) => "(" + printBSVExpr(e1) +
      " " + op + " " + printBSVExpr(e2) + ")"
    case ERecAccess(rec, fieldName) => "TODO"
    case ERecLiteral(fields) => "TODO"
    case EMemAccess(mem, index) => "TODO"
    case EBitExtract(num, start, end) => printBSVExpr(num) + "[" + end + ":" + start + "]"
    case ETernary(cond, tval, fval) => "case(" + printBSVExpr(cond) + ")" +
      "\n" + indent + indent + "True: return " + printBSVExpr(tval) + ";" +
      "\n" + indent + indent + "False: return " + printBSVExpr(fval) + ";" +
      "\n" + indent + "endcase;"
    case EApp(func, args) => "TODO F-APP"
    case EVar(id) => id.v
    case ECast(ctyp, exp) => "TODO CAST"
    case expr: CirExpr => "TODO"
  }
}
