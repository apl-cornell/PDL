package pipedsl.typechecker

import pipedsl.common.Errors.{IllegalLockAcquisition, IllegalLockRelease, InvalidLockState, UnexpectedCase}
import pipedsl.common.Locks
import pipedsl.common.Locks._
import pipedsl.common.Syntax._
import pipedsl.typechecker.Environments._
import pipedsl.typechecker.TypeChecker.TypeChecks
import z3.scala.{Z3AST, Z3Context, Z3Solver}

import scala.collection.mutable.Stack

/**
 * This checks that all reads and writes to memories
 * only happen when appropriate.
 * - Checks: Whenever a memory is read or written, the lock for that memory has been acquired
 * - Checks: That all locks are released (or never acquired) by the end of the program
 * When checking a lock state, it checks whether it is possible for the lock state to NOT be the expected. If it is
 * possible, the type checking fails.
 */
//TODO: Make error case classes
object LockConstraintChecker extends TypeChecks[Id, Z3AST] {
  
  val ctx: Z3Context = new Z3Context()
  val solver: Z3Solver = ctx.mkSolver()

  val predicates: Stack[Z3AST] = Stack(ctx.mkTrue())
  val predicateGenerator = new PredicateGenerator(ctx)
  
  var incrementer = 0
  
  override def emptyEnv(): Environment[Id, Z3AST] = ConditionalLockEnv(ctx = ctx)
  //Functions can't interact with locks or memories right now.
  //Could add that to the function types explicitly to be able to check applications
  override def checkFunc(f: FuncDef, env: Environment[Id, Z3AST]): Environment[Id, Z3AST] = env

  override def checkModule(m: ModuleDef, env: Environment[Id, Z3AST]): Environment[Id, Z3AST] = {
    val nenv = m.modules.foldLeft[Environment[Id, Z3AST]](env)( (e, m) => m.typ match {
      case TMemType(_, _, _, _) => e.add(m.name, makeEquals(m.name, Free))
      case TModType(_, _, _, _) => e.add(m.name, makeEquals(m.name, Free))
      case _ => throw UnexpectedCase(m.pos)
    })
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
  
  def checkCommand(c: Command, env: Environment[Id, Z3AST]): Environment[Id, Z3AST] = {
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
        val df = checkCommand(default, env)
        cases.foldLeft(df)((fenv, cs) => {
          fenv.intersect(checkCommand(cs.body, env))
        })
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
          .foldLeft[Map[Id, Z3AST]](Map())((nenv, id) => nenv + (id -> ctx.mkImplies(ctx.mkAnd(predicates.toSeq: _*), lt(id)))),
          ctx)
        val trueBranch = predicates.pop()
        
        predicates.push(ctx.mkNot(trueBranch))
        val lf = checkCommand(alt, env)
        //makes new environment with all locks implied by false branch
        val fenv = ConditionalLockEnv(lf.getMappedKeys()
          .foldLeft[Map[Id, Z3AST]](Map())((nenv, id) => nenv + (id -> ctx.mkImplies(ctx.mkAnd(predicates.toSeq: _*), lf(id)))),
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
        case (_, EMemAccess(mem,_)) => {
          checkState(mem, env, Acquired.order) match {
            case Some(value) if value => throw new RuntimeException("A possible thread of execution can cause this to fail: memories needs to be acquired before accessing")
            case None => throw new RuntimeException("AN error occurred while attempting to solve the constraints")
          }
          env
        }
        case _ => env
      }
      case CRecv(lhs, rhs) => (lhs, rhs) match {
        case (EMemAccess(mem,_), _) => {
          checkState(mem, env, Acquired.order) match {
            case Some(value) if value => throw new RuntimeException("A possible thread of execution can cause this to fail: memories needs to be acquired before accessing")
            case None => throw new RuntimeException("AN error occurred while attempting to solve the constraints")
            case _ => env
          }
          env
        }
        case (_, EMemAccess(mem,_)) => {
          checkState(mem, env, Acquired.order) match {
            case Some(value) if value => throw new RuntimeException("A possible thread of execution can cause this to fail: memories needs to be acquired before accessing")
            case None => throw new RuntimeException("AN error occurred while attempting to solve the constraints")
            case _ => env
          }
          env
        }
        case (_, ECall(mod,_)) => {
          checkState(mod, env, Acquired.order) match {
            case Some(value) if value => throw new RuntimeException(" A possible thread of execution can cause this to fail: modules needs to be acquired before accessing")
            case None => throw new RuntimeException("AN error occurred while attempting to solve the constraints")
            case _ => env
          }
          env
        }
        case _ => throw UnexpectedCase(c.pos)
      }
      //TODO don't just use id
      case CLockOp(mem, op) => op match {
        case Locks.Reserved => checkState(mem.id, env, Free.order) match {
          case Some(value) if value => throw new RuntimeException("A possible thread of execution can cause this to fail: memories needs to be free before reserving")
          case Some(value) if !value => env.add(mem.id,  ctx.mkImplies(ctx.mkAnd(predicates.toSeq: _*), makeEquals(mem.id, Reserved)))
          case None => throw new RuntimeException("An error occurred while attempting to solve the constraints")
        }
        case Locks.Acquired => checkState(mem.id, env, Reserved.order) match {
          case Some(value) if value => throw new RuntimeException("A possible thread of execution can cause this to fail: memories needs to be reserved before acquiring")
          case Some(value) if !value => env.add(mem.id,  ctx.mkImplies(ctx.mkAnd(predicates.toSeq: _*), makeEquals(mem.id, Acquired)))
          case None => throw new RuntimeException("An error occurred while attempting to solve the constraints")
        }
        case Locks.Released => checkState(mem.id, env, Acquired.order) match {
          case Some(value) if value => throw new RuntimeException("A possible thread of execution can cause this to fail: memories needs to be acquired before releasing")
          case Some(value) if !value => env.add(mem.id,  ctx.mkImplies(ctx.mkAnd(predicates.toSeq: _*), makeEquals(mem.id, Released)))
          case None => throw new RuntimeException("An error occurred while attempting to solve the constraints")
        }
      } //logic inside the lock environment class
      case _ => env
    }
  }
  override def checkCircuit(c: Circuit, env: Environment[Id, Z3AST]): Environment[Id, Z3AST] = env
  
  private def checkState(mem: Id, env: Environment[Id, Z3AST], lockStateOrders: Int*): Option[Boolean] = {
    //Makes an OR of all given lock states
    val stateAST = lockStateOrders.foldLeft(ctx.mkFalse())((ast, order) => ctx.mkOr(ast, ctx.mkEq(
      ctx.mkIntConst(mem.v),
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
  
  private def makeEquals(mem: Id, lockState: LockState): Z3AST = {
    ctx.mkEq(ctx.mkIntConst(mem.v), ctx.mkInt(lockState.order, ctx.mkIntSort()))
  }
}