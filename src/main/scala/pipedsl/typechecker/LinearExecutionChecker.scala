package pipedsl.typechecker

import com.microsoft.z3.{
  AST => Z3AST, BoolExpr => Z3BoolExpr, Context => Z3Context, Solver =>
  Z3Solver, Status => Z3Status
}
import pipedsl.common.Syntax._
import pipedsl.common.Errors._
import pipedsl.common.Utilities.{mkAnd, mkOr}
import pipedsl.typechecker.TypeChecker.TypeChecks

import scala.util.parsing.input.Position
import scala.collection.mutable

/**
 * On each program path in a pipeline you should either make a "recursive"
 * call or produce an output. Checks this.
 */
class LinearExecutionChecker(val ctx: Z3Context) extends TypeChecks[Id, Z3AST]
{

  private val solver: Z3Solver = ctx.mkSolver()

  private val predicates: mutable.Stack[Z3AST] = mutable.Stack(ctx.mkFalse())
  private var currentPipe: Id = null

  override def emptyEnv(): Environments.Environment[Id, Z3AST] = null

  override def checkExt(e: ExternDef,
    env: Environments.Environment[Id, Z3AST]): Environments.Environment[Id, Z3AST] = env

  /* functions do not have any out of order properties, so we don't need to */
  /* check them */
  override def checkFunc(f: FuncDef,
                         env: Environments.Environment[Id, Z3AST])
  : Environments.Environment[Id, Z3AST] = env

  override def checkModule(m: ModuleDef, env: Environments.Environment[Id,
    Z3AST]): Environments.Environment[Id, Z3AST] =
    {
      predicates.clear()
      predicates.push(ctx.mkFalse())
      currentPipe = m.name
      checkCommand(m.body)
      checkAllRecurse() match
      {
        case Z3Status.SATISFIABLE | Z3Status.UNKNOWN =>
          throw LonelyPaths(m.name)
        case _ =>
      }
      env
    }

  override def checkCircuit(c: Circuit, env: Environments.Environment[Id,
    Z3AST]): Environments.Environment[Id, Z3AST] = env

  def checkCommand(c: Command): Unit =
    {
      c match
      {
        case CSeq(c1, c2) => checkCommand(c1); checkCommand(c2)
        case CTBar(c1, c2) => checkCommand(c1); checkCommand(c2)
        case CIf(_, cons, alt) => checkCommand(cons); checkCommand(alt)
        case CSplit(cases, default) =>
          for (caseObj <- cases) checkCommand(caseObj.body)
          checkCommand(default)
        case COutput(_) => verifyRecursive(c.predicateCtx.get, c.pos)
        case CVerify(_, args, _, _) =>
          val pred = c.predicateCtx.get
          args.foreach(a => checkExpr(a, pred))
          verifyRecursive(c.predicateCtx.get, c.pos)
        case CAssign(_, rhs) => checkExpr(rhs, c.predicateCtx.get)
        case CExpr(exp) => checkExpr(exp, c.predicateCtx.get)
        case _ =>
      }
    }

  def checkExpr(e: Expr, predicate: Z3BoolExpr)
  : Unit = e match
  {
    case EUop(_, ex) => checkExpr(ex, predicate)
    case EBinop(_, e1, e2) => checkExpr(e1, predicate); checkExpr(e2, predicate)
    case ETernary(cond, tval, fval) =>
      checkExpr(cond, predicate)
      checkExpr(tval, predicate)
      checkExpr(fval, predicate)
    case EApp(_, args) =>
      args.foreach(e => checkExpr(e, predicate))
    case ECast(_, exp) => checkExpr(exp, predicate)
    case ECall(mod, name, args) =>
      args.foreach(a => checkExpr(a, predicate))
      if(mod == currentPipe)
        verifyRecursive(predicate, e.pos)
    case _ =>
  }

  /**
   * Checks to see if [[predicate]] can be true at the same time that ANY of
   * the globl [[predicates]] are in [[env]] */
  def checkRecursive(predicate: Z3BoolExpr): Z3Status =
    {
      /* we want to know if it is possible to satisfy the current predicate */
      /* we are testing AND ANY of the other known predicates */
      val or_stmt
      = mkOr(ctx, predicates.toSeq.map(ast => mkAnd(ctx, ast, predicate)): _*)
      solver.add(or_stmt)
      val check = solver.check()
      solver.reset()
      check
    }

  /**
   * call checkRecursive and throws the appropriate errors on output
   */
  def verifyRecursive(predicate: Z3BoolExpr, pos :Position) :Unit =
    checkRecursive(predicate) match
    {
      case Z3Status.SATISFIABLE => throw MultipleCall(pos, true)
      case Z3Status.UNKNOWN => throw MultipleCall(pos, false)
      case Z3Status.UNSATISFIABLE => predicates.push(predicate)
    }

  /**
   * Checks to see if all the predicates together form a tautology */
  def checkAllRecurse(): Z3Status =
    {
      solver.add(ctx.mkNot(mkOr(ctx, predicates.toSeq: _*)))
      val check = solver.check()
      solver.reset()
      check
    }
}
