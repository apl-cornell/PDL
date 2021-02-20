package pipedsl.passes

import pipedsl.common.Syntax._
import pipedsl.passes.Passes.{CommandPass, ModulePass, ProgPass}

import scala.collection.mutable.ListBuffer

object RemoveTimingPass extends CommandPass[Command] with ModulePass[ModuleDef] with ProgPass[Prog] {
  var calls: ListBuffer[Command] = new ListBuffer[Command]()
  
  override def run(p: Prog): Prog = {
    p.copy(moddefs = p.moddefs.map(m => run(m))).setPos(p.pos)
  }

  override def run(m: ModuleDef): ModuleDef = {
    m.copy(body = run(m.body)).setPos(m.pos)
  }

  override def run(c: Command): Command = {
    CSeq(removeTimingConstructs(c), convertCListToCSeq(calls, 0))
  }
  
  def removeTimingConstructs(c: Command): Command = {
    c match {
      case CSeq(c1, c2) => CSeq(removeTimingConstructs(c1), removeTimingConstructs(c2))
      case CIf(cond, cons, alt) => CIf(cond, removeTimingConstructs(cons), removeTimingConstructs(alt))
      case CTBar(c1, c2) => CSeq(removeTimingConstructs(c1), removeTimingConstructs(c2));
      case CLockOp(_,_) => CEmpty()
      case CSpeculate(_, _, _, _) => CEmpty()
      case CCheck(_) => CEmpty()
      case CSplit(cases, default) =>
        val newCases = List[CaseObj]()
        val newDefault = removeTimingConstructs(default)
        for (index <- cases.indices) {
          val newBody = removeTimingConstructs(cases(index).body)
          newCases :+ cases(index).copy(body = newBody)
        }
        CSplit(newCases, newDefault)
      case CExpr(ECall(id, args)) =>
        val assigns: ListBuffer[Command] = new ListBuffer[Command]()
        val newArgs: ListBuffer[Expr] = new ListBuffer[Expr]()
        for (index <- args.indices) {
          val arg = EVar(Id("__" + id.v + "__" + calls.length + index))
          assigns.addOne(CAssign(arg, args(index)))
          newArgs.addOne(arg)
        }
        calls.addOne(CExpr(ECall(id, newArgs.toList)))
        convertCListToCSeq(assigns, 0)
      case _ => c
    }
  }
  
  def convertCListToCSeq(commands: ListBuffer[Command], i: Int): Command = {
    if (i > commands.length-1) {
      CEmpty()
    } else {
      CSeq(commands(i), convertCListToCSeq(commands, i+1))
    }
  }
}
