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

  def printBSVCommand(c: Command, indent: String = ""): String = c match {
    case CSeq(c1, c2) => "SEQUENCE NOT EXPECTED"
    case CTBar(c1, c2) => "SEQUENCE NOT EXPECTED"
    case CIf(cond, cons, alt) => ""
    case CAssign(lhs, rhs) =>""
    case CRecv(lhs, rhs) =>""
    case CCall(id, args) =>""
    case COutput(exp) =>""
    case CReturn(exp) =>""
    case CExpr(exp) =>""
    case CLockOp(mem, op) =>""
    case CSpeculate(predVar, predVal, verify, body) =>""
    case CCheck(predVar) =>""
    case CSplit(cases, default) =>""
    case Syntax.CEmpty => ""
    case _ => "TODO / Uncompilable command"
  }
}
