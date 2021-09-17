package pipedsl.passes

import pipedsl.common.Syntax._
import pipedsl.passes.Passes.{ModulePass, ProgPass}

import scala.annotation.tailrec

/**
 * This pass updates the 'isRecursive' annotation on the
 * module definition. Modules are by default assumed to be
 * recursive since this is a safe assumption. This pass checks to
 * see if the module makes any recurisve calls and updates the
 * annotation to reflect that.
 *
 * This pass also marks recursive modules with a Speculative Annotation
 * if they ever use speculative calls.
 */
object MarkNonRecursiveModulePass extends ModulePass[ModuleDef] with ProgPass[Prog] {


  override def run(p: Prog): Prog = {
    p.moddefs.foreach(m => run(m))
    p
  }

  override def run(m: ModuleDef): ModuleDef = {
    val (isRec, isSpec) = hasRecursiveCall(m.body, m.name)
    m.isRecursive = isRec
    m.maybeSpec = isSpec
    m
  }

  private def or(l: (Boolean, Boolean), r: (Boolean, Boolean)): (Boolean, Boolean) = {
    (l._1 || r._1, l._2 || r._2)
  }

  private def hasRecursiveCall(c: Command, mod: Id): (Boolean, Boolean) = c match {
    case CSeq(c1, c2) => or(hasRecursiveCall(c1, mod), hasRecursiveCall(c2, mod))
    case CTBar(c1, c2) => or(hasRecursiveCall(c1, mod), hasRecursiveCall(c2, mod))
    case CIf(_, cons, alt) => or(hasRecursiveCall(cons, mod), hasRecursiveCall(alt, mod))
    case CSplit(cases, default) =>
      cases.foldLeft(hasRecursiveCall(default, mod))((b, c) => {
         or(b,or(hasRecursiveCall(c.body, mod), (hasRecCall(c.cond, mod), false)))
      })
    case CAssign(_, rhs) => (hasRecCall(rhs, mod), false)
    case CRecv(_, rhs) => (hasRecCall(rhs, mod), false)
    case COutput(exp) => (hasRecCall(exp, mod), false)
    case CExpr(exp) => (hasRecCall(exp, mod), false)
    case CSpecCall(_,pipe,_) if pipe == mod => (true, true)
    case _ => (false, false)
  }

  //Call expressions can only appear inside cast expressions,
  //no other subexpressions - the Timing Type Checker ensures this
  @tailrec
  private def hasRecCall(e: Expr, m: Id): Boolean = e match {
    case ECall(mod, _, _) => mod == m
    case ECast(_, exp) => hasRecCall(exp, m)
    case _ => false
  }
}
