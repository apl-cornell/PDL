package pipedsl.typechecker

import pipedsl.common.Errors.UnexpectedCase
import pipedsl.common.Locks
import pipedsl.common.Locks._
import pipedsl.common.Syntax._
import pipedsl.typechecker.Environments._
import pipedsl.typechecker.TypeChecker.TypeChecks
import z3.scala.{Z3AST, Z3Context, Z3Solver}

import scala.collection.mutable

/**
 * This checks that all reads and writes to memories
 * only happen when appropriate.
 * - Checks: Whenever a memory is read or written, the lock for that memory has been acquired
 * - Checks: That all locks are released (or never acquired) by the end of the program
 * When checking a lock state, it checks whether it is possible for the lock state to NOT be the expected. If it is
 * possible, the type checking fails.
 */
//TODO: Make error case classes
class LockConstraintChecker(lockMap: Map[Id, Set[LockArg]], lockTypeMap: Map[Id, Map[Id, LockType]]) extends TypeChecks[LockArg, Z3AST] {
  
  private val ctx: Z3Context = new Z3Context()
  private val solver: Z3Solver = ctx.mkSolver()

  private val predicates: mutable.Stack[Z3AST] = mutable.Stack(ctx.mkTrue())
  private val predicateGenerator = new PredicateGenerator(ctx)
  
  private var incrementer = 0

  private var currentMod = Id("-invalid-")

  override def emptyEnv(): Environment[LockArg, Z3AST] = ConditionalLockEnv(ctx = ctx)
  //Functions can't interact with locks or memories right now.
  //Could add that to the function types explicitly to be able to check applications
  override def checkFunc(f: FuncDef, env: Environment[LockArg, Z3AST]): Environment[LockArg, Z3AST] = env

  override def checkModule(m: ModuleDef, env: Environment[LockArg, Z3AST]): Environment[LockArg, Z3AST] = {
    currentMod = m.name
    val nenv = lockMap(m.name).foldLeft[Environment[LockArg, Z3AST]](env)((e, mem) => e.add(mem, makeEquals(mem, Free)))
    val finalenv = checkCommand(m.body, nenv)
    //At end of execution all locks must be free or released
    finalenv.getMappedKeys().foreach(id => {
      checkState(id, finalenv, Released.order, Free.order) match {
        case Some(value) if value => 
          throw new RuntimeException("We want everything at end to be free or released")
        case _ => 
      }
    })
    env //no change to lock map after checking module
  }
  
  def checkCommand(c: Command, env: Environment[LockArg, Z3AST]): Environment[LockArg, Z3AST] = {
    c match {
      case CSeq(c1, c2) => {
        val l1 = checkCommand(c1, env)
        checkCommand(c2, l1)
      }
      case CTBar(c1, c2) => {
        val l1 = checkCommand(c1, env)
        checkCommand(c2, l1)
      }
      case CSplit(cases, default) => {
        //This will keep track of the not predicates required for all previously seen cases
        val runningPredicates = mutable.Stack[Z3AST]()
        //keeps track of the current environment as iterate through the cases
        var runningEnv = env
        //need some special handling for the first case statement
        var first = true
        for (caseObj <- cases) {
          //get abstract interp of condition
          var currentCond: Z3AST = null
          predicateGenerator.abstractInterpExpr(caseObj.cond) match {
            case Some(value) => currentCond = value
            case None => currentCond = ctx.mkEq(ctx.mkBoolConst("__TOPCONSTANT__" + incrementer), ctx.mkTrue())
          }
          //Get the not of the current condition
          val notCurrentCond = ctx.mkNot(currentCond)
          if (runningPredicates.isEmpty) {
            predicates.push(currentCond)
            runningPredicates.push(notCurrentCond)
          } else {
            val runningNot = runningPredicates.pop()
            //need to add the current condition and the running Not of the previous cases to the predicates
            predicates.push(ctx.mkAnd(runningNot, currentCond))
            //add to the current running not
            runningPredicates.push(ctx.mkAnd(runningNot, notCurrentCond))
          }
          val newEnv = checkCommand(caseObj.body, env)

          //makes new environment with all locks implied by this case
          val tenv = ConditionalLockEnv(newEnv.getMappedKeys()
            .foldLeft[Map[LockArg, Z3AST]](Map())((nenv, id) => nenv + (id -> ctx.mkImplies(ctx.mkAnd(predicates.toSeq: _*), newEnv(id)))),
            ctx)
          //remove the predicate used for this case statement to reset for the next case
          predicates.pop()
          if (first) {
            runningEnv = tenv
          } else {
            runningEnv = runningEnv.intersect(tenv)
          }
          first = false
        }
        //For default, all the case statements must be false, so add this to the predicates
        predicates.push(runningPredicates.pop())
        val defEnv = checkCommand(default, env)
        val tenv = ConditionalLockEnv(defEnv.getMappedKeys()
          .foldLeft[Map[LockArg, Z3AST]](Map())((nenv, id) => nenv + (id -> ctx.mkImplies(ctx.mkAnd(predicates.toSeq: _*), defEnv(id)))),
          ctx)
        predicates.pop()
        tenv.intersect(runningEnv)
      }
      case CIf(expr, cons, alt) => {
        predicateGenerator.abstractInterpExpr(expr) match {
          case Some(value) => predicates.push(value);
          case None => predicates.push(ctx.mkEq(ctx.mkBoolConst("__TOPCONSTANT__" + incrementer), ctx.mkTrue()))
        }
        incrementer += 1
        
        val lt = checkCommand(cons, env)
        //makes new environment with all locks implied by true branch
        val tenv = ConditionalLockEnv(lt.getMappedKeys()
          .foldLeft[Map[LockArg, Z3AST]](Map())((nenv, id) => nenv + (id -> ctx.mkImplies(ctx.mkAnd(predicates.toSeq: _*), lt(id)))),
          ctx)
        val trueBranch = predicates.pop()
        
        predicates.push(ctx.mkNot(trueBranch))
        val lf = checkCommand(alt, env)
        //makes new environment with all locks implied by false branch
        val fenv = ConditionalLockEnv(lf.getMappedKeys()
          .foldLeft[Map[LockArg, Z3AST]](Map())((nenv, id) => nenv + (id -> ctx.mkImplies(ctx.mkAnd(predicates.toSeq: _*), lf(id)))),
          ctx)
        predicates.pop()
        
        //Merge the two envs
        tenv.intersect(fenv) //real merge logic lives inside Envrionments.Z3AST
      }
      case CSpeculate(predVar, predVal, verify, body) => {
        //TODO 
        env
      }
      case CAssign(lhs, rhs) => (lhs, rhs) match {
        case (_, EMemAccess(mem, expr)) => {
          checkAcquired(mem, expr, env)
        }
        case _ => env
      }
      case CRecv(lhs, rhs) => (lhs, rhs) match {
        case (EMemAccess(mem, expr), _) => {
          checkAcquired(mem, expr, env)
        }
        case (_, EMemAccess(mem, expr)) => {
          checkAcquired(mem, expr, env)
        }
        case (_, ECall(mod, _)) => {
          //TODO Maybe just from null
          checkAcquired(mod, null, env)
        }
        case _ => throw UnexpectedCase(c.pos)
      }
      case CLockOp(mem, op) => op match {
        case Locks.Free => env //unreachable
        case Locks.Reserved => checkState(mem, env, Free.order) match {
          case Some(value) if value => throw new RuntimeException("A possible thread of execution can cause this to fail: memories needs to be free before reserving")
          case Some(value) if !value => env.add(mem,  ctx.mkImplies(ctx.mkAnd(predicates.toSeq: _*), makeEquals(mem, Reserved)))
          case None => throw new RuntimeException("An error occurred while attempting to solve the constraints")
        }
        case Locks.Acquired => checkState(mem, env, Reserved.order) match {
          case Some(value) if value =>
            throw new RuntimeException("A possible thread of execution can cause this to fail: memories needs to be reserved before acquiring")
          case Some(value) if !value => env.add(mem,  ctx.mkImplies(ctx.mkAnd(predicates.toSeq: _*), makeEquals(mem, Acquired)))
          case None => throw new RuntimeException("An error occurred while attempting to solve the constraints")
        }
        case Locks.Released => checkState(mem, env, Acquired.order) match {
          case Some(value) if value =>
            throw new RuntimeException("A possible thread of execution can cause this to fail: memories needs to be acquired before releasing")
          case Some(value) if !value => env.add(mem, ctx.mkImplies(ctx.mkAnd(predicates.toSeq: _*), makeEquals(mem, Released)))
          case None => throw new RuntimeException("An error occurred while attempting to solve the constraints")
        }
      } //logic inside the lock environment class
      case _ => env
    }
  }
  override def checkCircuit(c: Circuit, env: Environment[LockArg, Z3AST]): Environment[LockArg, Z3AST] = env
  
  private def checkState(mem: LockArg, env: Environment[LockArg, Z3AST], lockStateOrders: Int*): Option[Boolean] = {
    //Makes an OR of all given lock states
    val stateAST = lockStateOrders.foldLeft(ctx.mkFalse())((ast, order) => ctx.mkOr(ast, ctx.mkEq(
      ctx.mkIntConst(constructVarName(mem)),
      ctx.mkInt(
        order,
        ctx.mkIntSort()))))
    
    // Makes all the current predicates true
    solver.assertCnstr(ctx.mkEq(ctx.mkAnd(predicates.toSeq: _*), ctx.mkTrue()))
    
    //Asserts the state of the lock currently, and checks if its possible for the mem to NOT be in the expected lock states
    solver.assertCnstr(ctx.mkAnd(
      env(mem),
      ctx.mkNot(stateAST)))
    val check = solver.check()
    solver.reset()
    check
  }
  
  private def makeEquals(mem: LockArg, lockState: LockState): Z3AST = {
    ctx.mkEq(ctx.mkIntConst(constructVarName(mem)), ctx.mkInt(lockState.order, ctx.mkIntSort()))
  }
  
  private def constructVarName(mem: LockArg): String = {
    mem.id + (if (mem.evar.isDefined) "[" + mem.evar.get.id.v + "]" else "")
  }
  
  private def checkAcquired(mem: Id, expr: Expr, env: Environment[LockArg, Z3AST]): Environment[LockArg, Z3AST] = {
    if (lockTypeMap(currentMod)(mem).equals(Specific) && !expr.isInstanceOf[EVar]) {
      throw new RuntimeException("We expect the argument in the memory access to be a variable")
    }
    checkState(if (lockTypeMap(currentMod)(mem).equals(General)) LockArg(mem, None) else LockArg(mem, Some(expr.asInstanceOf[EVar])),
      env, 
      Acquired.order) 
    match {
      case Some(value) if value => throw new RuntimeException("A possible thread of execution can cause this to fail: memories needs to be acquired before accessing")
      case None => throw new RuntimeException("AN error occurred while attempting to solve the constraints")
      case _ => env
    }
  }
}