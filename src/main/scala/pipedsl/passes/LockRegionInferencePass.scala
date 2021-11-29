package pipedsl.passes

import com.microsoft.z3.{AST => Z3AST, BoolExpr => Z3BoolExpr, Context => Z3Context, Solver => Z3Solver, Status => Z3Status}
import pipedsl.common.Syntax
import pipedsl.common.Syntax.{Command, ModuleDef, Prog}
import pipedsl.passes.Passes.{CommandPass, ModulePass, ProgPass}

/**
 * This class automatically infers where to place lock region start ([[pipedsl.common.Syntax.CLockStart(mod)]],
 * and end ([[pipedsl.common.Syntax.CLockEnd(mod)]]) statements.
 * These are inferred based on the following algorithm:
 *  - Place one [[pipedsl.common.Syntax.CLockStart]] statement for each memory, _as late as possible_ such that
 *  it still dominates all atomic operations and lock reservation statements for that memory.
 *  - Place one [[pipedsl.common.Syntax.CLockEnd]] statement for each memory, _as early as possible_ such tath
 *  it is still dominated by all atomic operations and lock reservation statements for that memory.
 *
 *  Additionally, place a [[pipedsl.common.Syntax.CCheckpoint]] statement right before the
 *  [[pipedsl.common.Syntax.CLockEnd]] statement, conditionally executed when checkpoints
 *  are necessary. Specifically, checkpoints are necessary on any program path when
 *  the same program path will lead to a [[pipedsl.common.Syntax.CInvalidate]],
 *  [[pipedsl.common.Syntax.CVerify]], or [[pipedsl.common.Syntax.CUpdate]] command.
 *
 *  We find these program paths by using Z3 to generate the set of conditions
 *  under which these are true and generate a condition by OR-ing them together.
 *
 * @param ctx
 */
class LockRegionInferencePass(val ctx: Z3Context)
  extends CommandPass[Command] with ModulePass[ModuleDef] with ProgPass[Prog] {

  override def run(m: ModuleDef): ModuleDef = m.copy(body = run(m.body)).setPos(m.pos)

  override def run(p: Prog): Prog = p.copy(exts = p.exts, fdefs = p.fdefs,
    moddefs = p.moddefs.map(m => run(m))).setPos(p.pos)

  override def run(c: Command): Command = ???
}
