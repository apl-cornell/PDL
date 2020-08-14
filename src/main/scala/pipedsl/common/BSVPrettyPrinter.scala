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
}
