package pipedsl.common

import pipedsl.common.DAGSyntax.{IfStage, PStage, PipelineEdge, SpecStage}
import pipedsl.common.Syntax._
import pipedsl.common.Utilities._
import pprint.pprintln

object PrettyPrinter {

  def printHelper(s: String, i: Int): Unit = pprintln((" " * i) + s)

  def printProgram(p: Prog): Unit = {
    p.fdefs.foreach(f => printFunction(f))
    p.moddefs.foreach(m => printModule(m))
    printCircuit(p.circ)
  }

  def printFunction(f: FuncDef): Unit = {
    pprintln("def " + f.name.v + "(" +
      f.args.map(printParamToString).mkString(",") +
      "):" + printTypeToString(f.ret) + "{\n" +
      printCmdToString(f.body, 2) + "\n}")
  }

  def printModule(m: ModuleDef): Unit = {
    pprintln("pipe " + m.name.v + "(" + m.inputs.map(printParamToString).mkString(",") +
      ")[" + m.modules.map(printParamToString).mkString(",") + "] {\n" +
    printCmdToString(m.body, 2) + "\n}")
  }

  def printCircuit(c: Circuit): Unit = pprintln("circuit {\n" + printCircuitToString(c, 2) + "\n}")

  def printCircuitToString(c: Circuit, indent: Int = 0): String = {
    val ins = " " * indent;
    c match {
      case CirSeq(c1, c2) =>
        printCircuitToString(c1, indent) + "\n" +
         printCircuitToString(c2, indent)
      case CirConnect(name, c) =>
        ins + name.v + " = " + printExprToString(c) + ";"
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
      case Syntax.CAssign(lhs, rhs) => ins + printTypeToString(lhs.typ.get) + " " + printExprToString(lhs) + " = " + printExprToString(rhs) + ";"
      case Syntax.CRecv(lhs, rhs) => ins + printTypeToString(lhs.typ.get) + " " + printExprToString(lhs) + " <- " + printExprToString(rhs) + ";"
      case Syntax.CCall(id, args) => ins + "call " + id + "(" +
        args.map(a => printExprToString(a)).mkString(",") + ");"
      case Syntax.COutput(exp) => ins + "output " + printExprToString(exp) + ";"
      case Syntax.CReturn(exp) => ins + "return " + printExprToString(exp) + ";"
      case Syntax.CExpr(exp) => ins + printExprToString(exp) + ";"
      case Syntax.CLockOp(mem, op) => ins + (op match {
        case pipedsl.common.Locks.LockState.Free => "free"
        case pipedsl.common.Locks.LockState.Reserved => "reserve"
        case pipedsl.common.Locks.LockState.Acquired => "acquire"
        case pipedsl.common.Locks.LockState.Released => "release"
      }) + "(" + mem.v + ");"
      case Syntax.CSpeculate(predVar, predVal, verify, body) => ins + "speculate (" +
        printTypeToString(predVar.typ.get) + " " + printExprToString(predVar) + " = " + printExprToString(predVal) + ", {\n" +
        printCmdToString(verify, indent + 4) + "\n" + ins + "}, {\n" +
        printCmdToString(body, indent + 4) + "\n" + ins + "}"
      case Syntax.CCheck(predVar) => ins + "check(" + predVar.v + ");"
      case Syntax.CEmpty => ins
      case Syntax.ICondCommand(cond, cmd) => ins + printExprToString(cond) + " ? " + printCmdToString(cmd)
      case Syntax.IUpdate(specId, value, originalSpec) => ins + printTypeToString(originalSpec.typ.get) + " " +
        printExprToString(originalSpec) + " = update(" + specId + ", " + printExprToString(value) + ");"
      case Syntax.ISpeculate(specId, specVar, value) => ins + specId + "= speculate(" + printExprToString(specVar) + ", " + printExprToString(value) + ");"
      case Syntax.ICheck(specId, value) => ins + "check(" + specId + ", " + printExprToString(value) + ");"
    }
  }

  def printExpr(e: Expr): Unit = pprintln(printExprToString(e))
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
    case Syntax.EVar(id) => id.v
    case Syntax.ECast(ctyp, exp) => "cast(" + printExprToString(exp) + "," + printTypeToString(ctyp) + ")"
    case expr: Syntax.CirExpr => expr match {
      case CirMem(elemTyp, addrSize) => "memory(" + printTypeToString(elemTyp) + "," + addrSize.toString + ")"
      case CirNew(mod, inits, mods) => "new " + mod.v +
        "(" + inits.map(i => printExprToString(i)).mkString(",") + ")" +
        "[" + mods.map(m => m.v).mkString(",") + "]"
    }
    case _ => "TODO"
  }

  def printType(t: Type): Unit = pprintln(printTypeToString(t))
  def printTypeToString(t: Type): String = t match {
    case TSizedInt(len, unsigned) => (if (!unsigned) "s" else "") + "int<" + len.toString + ">"
    case TVoid() => "void"
    case TBool() => "bool"
    case TFun(args, ret) => "(" + args.map(a => printTypeToString(a)).mkString(",") + ") -> " + printTypeToString(ret)
    case TRecType(name, fields) => name.v + " : " + "{ " + fields.keySet.map(f => f.v + ":" + fields(f)).mkString(",") + " }"
    case TMemType(elem, addrSize) =>printTypeToString(elem) + "[" + addrSize.toString + "]"
    case TModType(inputs, refs) => "TODO MOD TYPE"
  }

  def printStageGraph(name: String, stages: List[PStage]): Unit = {
    println("digraph " + name + " {")
    printStagesForDot(stages)
    println("}")
  }

  def printStagesForDot(stgs: List[PStage]): Unit = {
    stgs.foreach( stg => stg match {
      case s:IfStage => {
        println("  subgraph cluster__" + s.name + " {")
        println("style=filled;")
        println("color=lightgrey;")
        println("node [style=filled,color=white];")
        println("label = \"IF(" + printExprToString(s.cond) + ")\";")
        s.outEdges.foreach(e => printEdge(e))
        printStagesForDot(s.trueStages)
        printStagesForDot(s.falseStages)
        println("}")
      }
      case s:SpecStage => {
        println("  subgraph cluster__" + s.name + " {")
        println("style=filled;")
        println("color=pink;")
        println("node [style=filled,color=white];")
        println("label = \"Spec(" + printExprToString(s.specVar) + " = " + printExprToString(s.specVal) + ")\";")
        s.outEdges.foreach(e => printEdge(e))
        printStagesForDot(s.verifyStages)
        printStagesForDot(s.specStages)
        println("}")
      }
      case _ => {
        stg.outEdges.foreach(edge => {
          printEdge(edge)
        })
      }
    })
  }

/*  def printStageForDot(stg: PStage): List[PStage] = {
    stg match {
      case s:IfStage => {
        println("  subgraph cluster__" + s.name + " {")
        println("style=filled;")
        println("color=lightgrey;")
        println("node [style=filled,color=white];")
        println("label = \"IF(" + printExprToString(s.cond) + ")\";")
        var nexts: List[PStage] = printStageForDot(s.tblock) ++ printStageForDot(s.fblock)
        while (nexts.nonEmpty) {
          val n = nexts.head
          nexts = nexts.tail
          if (n != s.joinStage) {
            nexts = nexts ++ printStageForDot(n)
          }
        }
        println("}")
        List(s.joinStage)
      }
      case s:SpecStage => {
        println("  subgraph cluster__" + s.name + " {")
        println("style=filled;")
        println("color=pink;")
        println("node [style=filled,color=white];")
        println("label = \"Spec(" + printExprToString(s.specVar) + " = " + printExprToString(s.specVal) + ")\";")
        s.outEdges.foreach(e => printEdge(e))
        var nexts: List[PStage] =  printStageForDot(s.verify) ++ printStageForDot(s.spec)
        while (nexts.nonEmpty) {
          val n = nexts.head
          nexts = nexts.tail
          if (n != s.joinStage) {
            nexts = nexts ++ printStageForDot(n)
          }
        }
        println("}")
        List(s.joinStage)
      }
      case _ => {
        stg.outEdges.foreach(edge => {
          printEdge(edge)
        })
        stg.succs.toList
      }
    }
  }*/

  private def printEdge(edge: PipelineEdge) = {
    val condStr = if (edge.cond.isDefined) printExprToString(edge.cond.get) + " ? " else ""
    println("  " + edge.from.name + " -> " + edge.to.name + "[label = \"" + edge.values.mkString(",") + "\"];")
  }

  def printStages(stgs: List[PStage]): Unit = {
    stgs.foreach( stg => {
      val stagetyp = stg match {
        case _:IfStage => "If Stage"
        case _:SpecStage => "Speculation Stage"
        case _ => "Stage"
      }
      println(stagetyp + ": " + stg.name.v + ":\n")
      stg.cmds.foreach(c => {
        println(printCmdToString(c, 2));
      })
      stg match {
        case s:IfStage => {
          println("condition = " + printExprToString(s.cond))
          println("True block:"); printStages(s.trueStages)
          println("False block:"); printStages(s.falseStages)
        }
        case s:SpecStage => {
          println("predict " + printExprToString(s.specVar) + " = " + printExprToString(s.specVal))
          println("Verify Block: "); printStages(s.verifyStages)
          println("Speculate Block: "); printStages(s.specStages)
        }
        case _ => ()
      }
      println("Out Edges = " + stg.outEdges.foldLeft("")((str, edge) => {
        val condStr = if (edge.cond.isDefined) printExprToString(edge.cond.get) + " ? " else ""
        str + condStr + edge.to.name + ", "
      }))
    })
  }
}
