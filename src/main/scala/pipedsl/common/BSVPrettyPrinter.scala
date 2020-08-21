package pipedsl.common

import java.io.Writer

import pipedsl.common.BSVSyntax._
import pipedsl.common.Syntax._

object BSVPrettyPrinter {

  private def mkExprString(strs: String*): String = {
    strs.mkString(" ")
  }

  def toDeclString(v: BVar): String = {
    mkExprString(toBSVTypeStr(v.typ), v.name)
  }

  def toBSVTypeStr(t: BSVType): String = t match {
    case BStruct(name, fields) => "struct {" +
      fields.map(f => {
        toDeclString(f)
      }).mkString("; ") +
      "} " + name
    case BInterface(name, tparams) => "TODO print interface type"
    case BSizedInt(unsigned, size) =>
      if (unsigned) {
        "U"
      } else {
        ""
      } + "Int#(" + size + ")"
    case BBool() => "Bool"
  }

  class BSVPretyPrinterImpl {

    private val indentSize = 4
    private var curIndent = 0

    private def indent(): String = {
      " " * curIndent
    }
    private def incIndent(): Unit = {
      curIndent += indentSize
    }
    private def decIndent(): Unit = {
      curIndent -= indentSize
    }

    private def mkIndentedExpr(strs: String*): String = {
      indent() + strs.mkString(" ")
    }
    private def mkStatementString(strs: String*): String = {
      indent() + strs.mkString(" ") + "\n;"
    }

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
      case EBool(v) => if (v) {
        "True"
      } else {
        "False"
      }
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

    def printImport(imp: BImport, w: Writer): Unit = {
      w.write(mkStatementString("import", imp.name, ":: *"))
    }

    def printStructDef(sdef: BStructDef, w: Writer): Unit = {
      w.write(
        mkStatementString("typedef", toBSVTypeStr(sdef.typ),
          "deriving(", sdef.derives.mkString(","), ")"
        )
      )
    }

    def printBSVStatement(stmt: BStatement, w: Writer): Unit = {

    }

    def printBSVRule(rule: BRuleDef, w: Writer): Unit = {

    }

    //TODO - we don't generate any methods atm
    def printBSVMethod(method: BMethodDef, w: Writer): Unit = {}

    def printModule(mod: BModuleDef, w: Writer, synthesize: Boolean = false): Unit = {
      val params = mod.params.map(p => toDeclString(p)).mkString(",")
      //this just defines the interface this module implements,
      // the variable is necessary but unused
      val interfaceParam = if (mod.typ.isDefined) {
        mod.typ.get.name + " _unused_"
      } else {
        "Empty" + " _unused_"
      }
      val paramString = "(" + params + ", " + interfaceParam + ")"
      if (synthesize) {
        w.write("(* synthesize *)\n")
      }
      w.write(mkStatementString("module", paramString))
      incIndent()
      mod.body.foreach(s => printBSVStatement(s, w))
      mkStatementString("") //for readability only
      mod.rules.foreach(r => printBSVRule(r, w))
      mkStatementString("")
      mod.methods.foreach(m => printBSVMethod(m, w))
      mkStatementString("")
      decIndent()
      //Doesn't end in semi-colon
      w.write(mkIndentedExpr("endmodule"))
    }

    def printBSVProg(b: BProgram, writer: Writer): Unit = {
      b.imports.foreach(i => printImport(i, writer))
      b.structs.foreach(s => printStructDef(s, writer))
      b.modules.foreach(m => printModule(m, writer))
      //TODO change topmodule to an actual module def
      writer.write(b.topModule)
    }
  }
}
