package pipedsl.common

import java.io.{File, FileOutputStream, OutputStreamWriter}

import pipedsl.common.DAGSyntax.{IfStage, PStage, PipelineEdge, SpecStage}
import pipedsl.common.Errors.UnexpectedType
import pipedsl.common.Syntax._

class PrettyPrinter(output: Option[File]) {

  private val writer = if (output.isDefined) {
    Some(new OutputStreamWriter(new FileOutputStream(output.get)))
  } else {
    None
  }

  def pline(str: String): Unit = {
    if (writer.isDefined) {
      writer.get.write(str + "\n")
      writer.get.flush()
    } else {
      println(str)
    }
  }

  def printHelper(s: String, i: Int): Unit = pline((" " * i) + s)

  def printProgram(p: Prog): Unit = {
    p.fdefs.foreach(f => printFunction(f))
    p.moddefs.foreach(m => printModule(m))
    printCircuit(p.circ)
  }

  def printFunction(f: FuncDef): Unit = {
    pline("def " + f.name.v + "(" +
      f.args.map(printParamToString).mkString(",") +
      "):" + printTypeToString(f.ret) + "{\n" +
      printCmdToString(f.body, 2) + "\n}")
  }

  def printModule(m: ModuleDef): Unit = {
    pline("pipe " + m.name.v + "(" + m.inputs.map(printParamToString).mkString(",") +
      ")[" + m.modules.map(printParamToString).mkString(",") + "] {\n" +
    printCmdToString(m.body, 2) + "\n}")
  }

  def printCircuit(c: Circuit): Unit = pline("circuit {\n" + printCircuitToString(c, 2) + "\n}")

  def printCircuitToString(c: Circuit, indent: Int = 0): String = {
    val ins = " " * indent
    c match {
      case CirSeq(c1, c2) =>
        printCircuitToString(c1, indent) + "\n" +
         printCircuitToString(c2, indent)
      case CirConnect(name, c) =>
        ins + name.v + " = " + printExprToString(c) + ";"
      case CirExprStmt(ce) => printExprToString(ce) + ";"
    }
  }


  def printParamToString(p: Param): String = p.name.v + ":" + printTypeToString(p.typ)

  def printCmd(c: Command, indent: Int = 0): Unit = printHelper(printCmdToString(c, indent), indent)
  def printCmdToString(c: Command, indent: Int = 0): String = {
    val ins: String = " " * indent
    c match {
      case Syntax.CSeq(c1, c2) =>
        printCmdToString(c1, indent) + "\n" +
          printCmdToString(c2, indent)
      case Syntax.CTBar(c1, c2) =>
        printCmdToString(c1, indent) + "\n" + ins + "---\n" +
          printCmdToString(c2, indent)
      case Syntax.CSplit(cases, default) =>
        val cins = ins + (" " * 2)
        val res = ins + "split {\n"
        val casestr = cases.foldLeft(res)((str, cs) => {
          str + cins + "case: " + printExprToString(cs.cond) + " {\n" +
          printCmdToString(cs.body, indent + 6) + "\n" + cins + "}\n"
        })
        casestr + cins + "default: {\n" + printCmdToString(default, indent + 6) +
        "\n" + cins + "}\n" + ins + "}"
      case Syntax.CIf(cond, cons, alt) =>
        ins + "if ( " + printExprToString(cond) + " ) {\n" +
          printCmdToString(cons, indent + 4) + "\n" + ins + "} else {\n" +
          printCmdToString(alt, indent + 4) + "\n" + ins + "}"

      case Syntax.CAssign(lhs, rhs) => ins + (if (lhs.typ.isDefined) printTypeToString(lhs.typ.get) + " " else "") +
        printExprToString(lhs) + " = " + printExprToString(rhs) + ";"
      case Syntax.CRecv(lhs, rhs) => ins + (if (lhs.typ.isDefined) printTypeToString(lhs.typ.get) + " " else "") + 
        printExprToString(lhs) + " <- " + printExprToString(rhs) + ";"
      case Syntax.COutput(exp) => ins + "output " + printExprToString(exp) + ";"
      case Syntax.CReturn(exp) => ins + "return " + printExprToString(exp) + ";"
      case Syntax.CExpr(exp) => ins + printExprToString(exp) + ";"
      case Syntax.CLockOp(mem, op, t) => ins + op.name + "(" + mem.id.v + (if (mem.evar.isDefined) "[" +
        printExprToString(mem.evar.get) + "]" else "") + (if (t.isDefined) "," + (if (t.get == LockRead) "R" else "W") else "") + ");"

      case Syntax.CLockStart(mod) => ins + "start(" + mod.v + ");"
      case Syntax.CLockEnd(mod) => ins + "end(" + mod.v + ");"
      case Syntax.CSpeculate(predVar, predVal, verify, body) => ins + "speculate (" +
        printTypeToString(predVar.typ.get) + " " + printExprToString(predVar) + " = " + 
        printExprToString(predVal) + ", {\n" +
        printCmdToString(verify, indent + 4) + "\n" + ins + "}, {\n" +
        printCmdToString(body, indent + 4) + "\n" + ins + "}"
      case Syntax.CCheck(predVar) => ins + "check(" + predVar.v + ");"
      case Syntax.CPrint(evar) => ins + "print(" + printExprToString(evar) + ");"
      case Syntax.CEmpty() => ins
      case Syntax.ICondCommand(cond, cmd) => ins + printExprToString(cond) + " ? " +
        cmd.foldLeft("")((s, c) => s + printCmdToString(c))
      case Syntax.IUpdate(specId, value, originalSpec) => ins + printTypeToString(originalSpec.typ.get) + " " +
        printExprToString(originalSpec) + " = update(" + specId + ", " + printExprToString(value) + ");"
      case Syntax.ISpeculate(specId, specVar, value) => ins + specId + "= speculate(" + printExprToString(specVar) + ", " + 
        printExprToString(value) + ");"
      case Syntax.ICheck(specId, value) => ins + "check(" + specId + ", " + printExprToString(value) + ");"
      case _ => "TODO PRINTING COMMAND"
    }
  }

  def printExpr(e: Expr): Unit = pline(printExprToString(e))
  def printExprToString(e: Expr): String = e match {
    case Syntax.EInt(v, base, bits) => (base match {
      case 2 => "0b" + v.toBinaryString
      case 8 => "0" + v.toOctalString
      case 10 => v.toString
      case 16 => "0x" + v.toHexString
    }) + "<" + bits.toString + ">"
    case Syntax.EBool(v) => v.toString
    case Syntax.EUop(op, ex) => op.op + printExprToString(ex)
    case Syntax.EBinop(op, e1, e2) => printExprToString(e1) + " " + op.op + " " + printExprToString(e2)
    case Syntax.ERecAccess(rec, fieldName) => printExprToString(rec) + "." + fieldName
    case Syntax.ERecLiteral(fields) => "{" + fields.keySet.map(i => i.v + printExprToString(fields(i))).mkString(",") + "}"
    case Syntax.EMemAccess(mem, index) => mem.v + "[" + printExprToString(index) + "]"
    case Syntax.EBitExtract(num, start, end) => printExprToString(num) + "{" + end.toString + ":" + start.toString + "}"
    case Syntax.ETernary(cond, tval, fval) => printExprToString(cond) + " ? " + printExprToString(tval) + " : " + printExprToString(fval)
    case Syntax.EApp(func, args) => func.v + "(" + args.map(a => printExprToString(a)).mkString(",") + ")"
    case Syntax.ECall(id, args) => "call " + id + "(" + args.map(a => printExprToString(a)).mkString(",") + ")"
    case Syntax.EVar(id) => id.v
    case Syntax.ECast(ctyp, exp) => "cast(" + printExprToString(exp) + "," + printTypeToString(ctyp) + ")"
    case expr: Syntax.CirExpr => expr match {
      case CirMem(elemTyp, addrSize) => "memory(" + printTypeToString(elemTyp) + "," + addrSize.toString + ")"
      case CirLockMem(elemTyp, addrSize, _, sz) => "memlock(" +
        printTypeToString(elemTyp) + "," + addrSize.toString + "," + sz.map(a => a.toString).mkString(",") + ")"
      case CirRegFile(elemTyp, addrSize) => "regfile(" + printTypeToString(elemTyp) + "," + addrSize.toString + ")"
      case CirLockRegFile(elemTyp, addrSize, _, sz) => "rflock(" +
        printTypeToString(elemTyp) + "," + addrSize.toString + "," + sz.map(a => a.toString).mkString(",") + ")"
      case CirLock(mem, impl, sz) => impl.toString + "(" + mem.v + ")" +
        "<" + sz.map(a => a.toString).mkString(",") + ">"
      case CirNew(mod, mods) => "new " + mod.v +
        "[" + mods.map(m => m.v).mkString(",") + "]"
      case CirCall(mod, args) => "call " + mod.v + "(" + args.map(a => printExprToString(a)).mkString(",") + ")"
    }
    case _ => "TODO"
  }

  def printType(t: Type): Unit = pline(printTypeToString(t))
  def printTypeToString(t: Type): String = t match {
    case TSizedInt(len, unsigned) => (if (!unsigned) "s" else "") + "int<" + len.toString + ">"
    case TVoid() => "void"
    case TBool() => "bool"
    case TFun(args, ret) => "(" + args.map(a => printTypeToString(a)).mkString(",") + ") -> " + printTypeToString(ret)
    case TRecType(name, fields) => name.v + " : " + "{ " + fields.keySet.map(f => f.v + ":" + fields(f)).mkString(",") + " }"
    case TMemType(elem, addrSize, rlat, wlat) => printTypeToString(elem) + "[" + addrSize.toString + "]" +
      "<" + rlat + ", " + wlat + ">"
    case TModType(_, _, _, _) => "TODO MOD TYPE"
    case TNamedType(name) =>  name.v
    case _ => throw UnexpectedType(t.pos, "pretty printing", "unimplemented", t)
  }

  def printStageGraph(name: String, stages: List[PStage]): Unit = {
    pline("digraph " + name + " {")
    printStagesForDot(stages)
    pline("}")
  }

  def printStagesForDot(stgs: List[PStage]): Unit = {
    stgs.foreach {
      case s: IfStage =>
        pline("  subgraph cluster__" + s.name + " {")
        pline("style=filled;")
        pline("color=lightgrey;")
        pline("node [style=filled,color=white];")
        // TODO: Not sure how to change this line
        // pline("label = \"IF(" + printExprToString(s.conds(0)) + ")\";")
        s.outEdges.foreach(e => printEdge(e))
        s.condStages.foreach(stg => printStagesForDot(stg))
        printStagesForDot(s.defaultStages)
        pline("}")
      case s: SpecStage =>
        pline("  subgraph cluster__" + s.name + " {")
        pline("style=filled;")
        pline("color=pink;")
        pline("node [style=filled,color=white];")
        pline("label = \"Spec(" + printExprToString(s.specVar) + " = " + printExprToString(s.specVal) + ")\";")
        s.outEdges.foreach(e => printEdge(e))
        printStagesForDot(s.verifyStages)
        printStagesForDot(s.specStages)
        pline("}")
      case stg =>
        stg.outEdges.foreach(edge => {
          printEdge(edge)
        })
    }
  }

  private def printEdge(edge: PipelineEdge): Unit = {
    val condStr = if (edge.condSend.isDefined) printExprToString(edge.condSend.get) + " ? " else ""
    pline("  " + edge.from.name + " -> " + edge.to.name +
      "[label = \"" + condStr + edge.values.mkString(",") + "\"];")
  }

  def printStages(stgs: List[PStage]): Unit = {
    stgs.foreach( stg => {
      val stagetyp = stg match {
        case _:IfStage => "If Stage"
        case _:SpecStage => "Speculation Stage"
        case _ => "Stage"
      }
      pline(stagetyp + ": " + stg.name.v + ":\n")
      stg.getCmds.foreach(c => {
        pline(printCmdToString(c, 2))
      })
      stg match {
        case s:IfStage =>
          pline("condition = " + printExprToString(s.conds(0)))
          pline("True block:")
          printStages(s.condStages(0))
          pline("False block:")
          printStages(s.defaultStages)
        case s:SpecStage =>
          pline("predict " + printExprToString(s.specVar) + " = " + printExprToString(s.specVal))
          pline("Verify Block: ")
          printStages(s.verifyStages)
          pline("Speculate Block: ")
          printStages(s.specStages)
        case _ => ()
      }
      pline("Out Edges = " + stg.outEdges.foldLeft("")((str, edge) => {
        val condStr = if (edge.condSend.isDefined) printExprToString(edge.condSend.get) + " ? " else ""
        str + condStr + edge.to.name + ", "
      }))
    })
  }
}
