/* CheckpointChecker.scala */
package pipedsl.typechecker

import com.microsoft.z3.{AST => Z3AST, BoolExpr => Z3BoolExpr, Context => Z3Context, Solver => Z3Solver, Status => Z3Status}
import pipedsl.common.Syntax
import pipedsl.common.Syntax.Id
import pipedsl.typechecker.Environments.ConditionalEnv
import pipedsl.typechecker.TypeChecker.TypeChecks

/**
 * This class checks that checkpoints are instantiated in the correct context.
 * - If a pipeline may speculatively reserve locks, then it must have a checkpoint for that memory.
 * - Checkpoints must be instantiated in the context in which they are used (i.e., with the necessary branch conditions)
 * - Checkpoints must be resolved by some Verify or Invalidate
 * @param ctx
 */
class CheckpointChecker(val ctx: Z3Context) extends TypeChecks[Id, Z3AST] {

  override def emptyEnv(): Environments.Environment[Id, Z3AST] = ConditionalEnv(ctx = ctx)

  override def checkExt(e: Syntax.ExternDef, env: Environments.Environment[Id, Z3AST]): Environments.Environment[Id, Z3AST] = ???

  override def checkFunc(f: Syntax.FuncDef, env: Environments.Environment[Id, Z3AST]): Environments.Environment[Id, Z3AST] = ???

  override def checkModule(m: Syntax.ModuleDef, env: Environments.Environment[Id, Z3AST]): Environments.Environment[Id, Z3AST] = ???

  override def checkCircuit(c: Syntax.Circuit, env: Environments.Environment[Id, Z3AST]): Environments.Environment[Id, Z3AST] = ???
}
