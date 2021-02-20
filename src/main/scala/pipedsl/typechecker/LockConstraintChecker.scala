package pipedsl.typechecker

import com.microsoft.z3.{AST => Z3AST, BoolExpr => Z3BoolExpr, Context => Z3Context, Solver => Z3Solver, Status => Z3Status}
import pipedsl.common.Errors.UnexpectedCase
import pipedsl.common.Locks
import pipedsl.common.Locks._
import pipedsl.common.Syntax._
import pipedsl.common.Utilities.{mkAnd, mkImplies}
import pipedsl.typechecker.Environments._
import pipedsl.typechecker.TypeChecker.TypeChecks

/**
 * This checks that all reads and writes to memories
 * only happen when appropriate.
 * - Checks: Whenever a memory is read or written, the lock for that memory has been acquired
 * - Checks: That all locks are released (or never acquired) by the end of the program
 * When checking a lock state, it checks whether it is possible for the lock state to NOT be the expected. If it is
 * possible, the type checking fails.
 */
//TODO: Make error case classes
class LockConstraintChecker(lockMap: Map[Id, Set[LockArg]], lockTypeMap: Map[Id, Map[Id, LockType]], val ctx: Z3Context)
  extends TypeChecks[LockArg, Z3AST] {
  private val solver: Z3Solver = ctx.mkSolver()
  private var currentMod = Id("-invalid-")

  override def emptyEnv(): Environment[LockArg, Z3AST] = ConditionalLockEnv(ctx = ctx)
  //Functions can't interact with locks or memories right now.
  //Could add that to the function types explicitly to be able to check applications
  override def checkFunc(f: FuncDef, env: Environment[LockArg, Z3AST]): Environment[LockArg, Z3AST] = env

  override def checkModule(m: ModuleDef, env: Environment[LockArg, Z3AST]): Environment[LockArg, Z3AST] = {
    currentMod = m.name
    val nenv = lockMap(m.name).foldLeft[Environment[LockArg, Z3AST]](emptyEnv())((e, mem) => e.add(mem, makeEquals(mem, Free)))
    val finalenv = checkCommand(m.body, nenv)
    //At end of execution all locks must be free or released
    finalenv.getMappedKeys().foreach(id => {
      checkState(id, finalenv, ctx.mkTrue(), Released.order, Free.order) match {
        case Z3Status.SATISFIABLE =>
          throw new RuntimeException("We want everything at end to be free or released")
        case _ => 
      }
    })
    env //no change to lock map after checking module
  }
  
  def checkCommand(c: Command, env: Environment[LockArg, Z3AST]): Environment[LockArg, Z3AST] = {
    c match {
      case CSeq(c1, c2) =>
        val l1 = checkCommand(c1, env)
        checkCommand(c2, l1)

      case CTBar(c1, c2) =>
        val l1 = checkCommand(c1, env)
        checkCommand(c2, l1)

      case CSplit(cases, default) =>
        //keeps track of the current environment as iterate through the cases
        var runningEnv = env
        //need some special handling for the first case statement
        var first = true
        for (caseObj <- cases) {
          val newEnv = checkCommand(caseObj.body, env)
          //makes new environment with all locks implied by this case
          val tenv = ConditionalLockEnv(newEnv.getMappedKeys()
            .foldLeft[Map[LockArg, Z3AST]](Map())((nenv, id) => nenv + (id -> mkImplies(ctx, caseObj.body.predicateCtx.get, newEnv(id)))),
            ctx)
          if (first) {
            runningEnv = tenv
          } else {
            runningEnv = runningEnv.intersect(tenv)
          }
          first = false
        }
        val defEnv = checkCommand(default, env)
        val tenv = ConditionalLockEnv(defEnv.getMappedKeys()
          .foldLeft[Map[LockArg, Z3AST]](Map())((nenv, id) => nenv + (id -> mkImplies(ctx, mkAnd(ctx, default.predicateCtx.get), defEnv(id)))),
          ctx)
        tenv.intersect(runningEnv)

      case CIf(expr, cons, alt) =>
        val lt = checkCommand(cons, env)
        //makes new environment with all locks implied by true branch
        val tenv = ConditionalLockEnv(lt.getMappedKeys()
          .foldLeft[Map[LockArg, Z3AST]](Map())((nenv, id) => nenv + (id -> mkImplies(ctx, cons.predicateCtx.get, lt(id)))),
          ctx)

        val lf = checkCommand(alt, env)
        //makes new environment with all locks implied by false branch
        val fenv = ConditionalLockEnv(lf.getMappedKeys()
          .foldLeft[Map[LockArg, Z3AST]](Map())((nenv, id) => nenv + (id -> mkImplies(ctx, alt.predicateCtx.get, lf(id)))),
          ctx)
        //Merge the two envs
        tenv.intersect(fenv) //real merge logic lives inside Envrionments.Z3AST

      case _: CSpeculate =>
        //TODO
        env
      case CAssign(_, rhs) => checkExpr(rhs, env, c.predicateCtx.get)
      case CRecv(lhs, rhs) => (lhs, rhs) match {
        case (EMemAccess(mem, expr), _) =>
          checkAcquired(mem, expr, env, c.predicateCtx.get)
        case (_, EMemAccess(mem, expr)) =>
          checkAcquired(mem, expr, env, c.predicateCtx.get)
        case (_, ECall(mod, _)) =>
          //TODO Maybe just from null
          checkAcquired(mod, null, env, c.predicateCtx.get)
        case _ => throw UnexpectedCase(c.pos)
      }

      case c@CLockOp(mem, op) =>
        val expectedLockState = op match {
          case Locks.Free => throw new IllegalStateException() // TODO: is this right?
          case Locks.Reserved => Free
          case Locks.Acquired => Reserved
          case Locks.Released => Acquired
        }
      checkState(mem, env, c.predicateCtx.get, expectedLockState.order) match {
        case Z3Status.UNSATISFIABLE =>
          env.add(mem, mkImplies(ctx, c.predicateCtx.get, makeEquals(mem, op)))
        case Z3Status.UNKNOWN =>
          throw new RuntimeException("An error occurred while attempting to solve the constraints")
        case Z3Status.SATISFIABLE =>
          throw new RuntimeException(s"A possible thread of execution can cause this to fail: memories needs to be $expectedLockState before $op")
      }

      case _ => env
    }
  }
  private def checkExpr(e: Expr, env: Environment[LockArg, Z3AST], predicates: Z3BoolExpr): Environment[LockArg, Z3AST] = e match {
    case EUop(_, ex) => checkExpr(ex, env, predicates)
    case EBinop(_, e1, e2) =>
      val env1 = checkExpr(e1, env, predicates)
      checkExpr(e2, env1, predicates)
    case EMemAccess(mem, index) => checkAcquired(mem, index, env, predicates)
    case ETernary(cond, tval, fval) =>
      val env1 = checkExpr(cond, env, predicates)
      val env2 = checkExpr(tval, env1, predicates)
      checkExpr(fval, env2, predicates)
    case EApp(_, args) => args.foldLeft(env)((e, a) => checkExpr(a, e, predicates))
    case ECall(_, args) => args.foldLeft(env)((e, a) => checkExpr(a, e, predicates))
    case ECast(_, exp) => checkExpr(exp, env, predicates)
    case _ => env
  }
  override def checkCircuit(c: Circuit, env: Environment[LockArg, Z3AST]): Environment[LockArg, Z3AST] = env
  
  private def checkState(mem: LockArg, env: Environment[LockArg, Z3AST], predicates: Z3BoolExpr, lockStateOrders: Int*): Z3Status = {

  // Makes an OR of all given lock states
    val stateAST = lockStateOrders.foldLeft(ctx.mkFalse())((ast, order) =>
      ctx.mkOr(ast, ctx.mkEq(ctx.mkIntConst(constructVarName(mem)), ctx.mkInt(order))))
    
    // Makes all the current predicates true
    solver.add(ctx.mkEq(predicates, ctx.mkTrue()))

    // Asserts the state of the lock currently, and checks if its possible for the mem to NOT be in the expected lock states
    solver.add(mkAnd(ctx, env(mem), ctx.mkNot(stateAST)))
    val check = solver.check()
    solver.reset()
    check
  }
  
  private def makeEquals(mem: LockArg, lockState: LockState): Z3AST = {
    ctx.mkEq(ctx.mkIntConst(constructVarName(mem)), ctx.mkInt(lockState.order))
  }
  
  private def constructVarName(mem: LockArg): String = {
    mem.id + (if (mem.evar.isDefined) "[" + mem.evar.get.id.v + "]" else "")
  }
  
  private def checkAcquired(mem: Id, expr: Expr, env: Environment[LockArg, Z3AST], predicates: Z3BoolExpr): Environment[LockArg, Z3AST] = {
    if (lockTypeMap(currentMod)(mem).equals(Specific) && !expr.isInstanceOf[EVar]) {
      throw new RuntimeException("We expect the argument in the memory access to be a variable")
    }
    checkState(if (lockTypeMap(currentMod)(mem).equals(General)) LockArg(mem, None) else LockArg(mem, Some(expr.asInstanceOf[EVar])),
      env,
      predicates,
      Acquired.order)
    match {
      case Z3Status.SATISFIABLE =>
        throw new RuntimeException("A possible thread of execution can cause this to fail: memories needs to be acquired before accessing")
      case Z3Status.UNKNOWN =>
        throw new RuntimeException("An error occurred while attempting to solve the constraints")
      case Z3Status.UNSATISFIABLE =>
        env
    }
  }
}