package pipedsl.passes

import pipedsl.common.Syntax._
import pipedsl.passes.Passes.{ModulePass, ProgPass}

/**
 * This pass updates the 'isRecursive' annotation on the
 * module definition. Modules are by default assumed to be
 * recursive since this is a safe assumption. This pass checks to
 * see if the module makes any recurisve calls and updates the
 * annotation to reflect that.
 */
object MarkNonRecursiveModulePass extends ModulePass[ModuleDef] with ProgPass[Prog] {


  override def run(p: Prog): Prog = {
    p.moddefs.foreach(m => run(m))
    p
  }

  override def run(m: ModuleDef): ModuleDef = {
    m.isRecursive = hasRecursiveCall(m.body, m.name)
    m
  }

  private def hasRecursiveCall(c: Command, mod: Id): Boolean = c match {
    case CSeq(c1, c2) => hasRecursiveCall(c1, mod) || hasRecursiveCall(c2, mod)
    case CTBar(c1, c2) => hasRecursiveCall(c1, mod) || hasRecursiveCall(c2, mod)
    case CIf(_, cons, alt) => hasRecursiveCall(cons, mod) || hasRecursiveCall(alt, mod)
    case CAssign(_, rhs) => hasRecCall(rhs, mod)
    case CRecv(_, rhs) => hasRecCall(rhs, mod)
    case COutput(exp) => hasRecCall(exp, mod)
    case CExpr(exp) => hasRecCall(exp, mod)
    case _ => false
  }

  //Call expressions can only appear inside cast expressions,
  //no other subexpressions - the Timing Type Checker ensures this
  private def hasRecCall(e: Expr, m: Id): Boolean = e match {
    case ECall(mod, _) => mod == m
    case ECast(_, exp) => hasRecCall(exp, m)
    case _ => false
  }
}
