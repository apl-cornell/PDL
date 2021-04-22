package pipedsl.typechecker

import com.microsoft.z3.{AST => Z3AST, BoolExpr => Z3BoolExpr, Context => Z3Context, Solver => Z3Solver, Status => Z3Status}
import pipedsl.common.Errors.UnexpectedCase
import pipedsl.common.Locks
import pipedsl.common.Locks._
import pipedsl.common.Syntax._
import pipedsl.common.Utilities.updateSetMap
import pipedsl.typechecker.Environments._
import pipedsl.typechecker.TypeChecker.TypeChecks

import scala.collection.mutable

/**
 * This checks that all reads and writes to memories
 * only happen when appropriate.
 * - Checks: Whenever a memory is read or written, the lock for that memory has been acquired
 * - Checks: That all locks are released (or never acquired) by the end of the program
 * - Checks: That all read locks are reserved before any write locks
 * - Checks: That all read locks are released before any write locks
 * When checking a lock state, it checks whether it is possible for the lock state to NOT be the expected. If it is
 * possible, the type checking fails.
 */
//TODO: Make error case classes
class LockConstraintChecker(lockMap: Map[Id, Set[LockArg]], lockTypeMap: Map[Id, Map[Id, LockGranularity]]) extends TypeChecks[LockArg, Z3AST] {
  
  private val ctx: Z3Context = new Z3Context()
  private val solver: Z3Solver = ctx.mkSolver()

  private val predicates: mutable.Stack[Z3AST] = mutable.Stack(ctx.mkTrue())
  private val predicateGenerator = new PredicateGenerator(ctx)

  private val lockReserveMode = ctx.mkIntConst("ReservedMode")
  private val lockReleaseMode = ctx.mkIntConst("ReleasedMode")

  private var topLevelReserveModeMap: Map[Id, Z3BoolExpr] = Map()
  private var topLevelReleaseModeMap: Map[Id, Z3BoolExpr] = Map()
  private var SMTReserveModeListMap: Map[Id, Set[Z3BoolExpr]] = Map()
  private var SMTReleaseModeListMap: Map[Id, Set[Z3BoolExpr]] = Map()
  private val READ = 0
  private val WRITE = 1

  private var incrementer = 0

  private var currentMod = Id("-invalid-")

  override def emptyEnv(): Environment[LockArg, Z3AST] = ConditionalLockEnv(ctx = ctx)
  //Functions can't interact with locks or memories right now.
  //Could add that to the function types explicitly to be able to check applications
  override def checkFunc(f: FuncDef, env: Environment[LockArg, Z3AST]): Environment[LockArg, Z3AST] = env

  override def checkModule(m: ModuleDef, env: Environment[LockArg, Z3AST]): Environment[LockArg, Z3AST] = {
    //Reads must go before writes, so the modes are initialized to read
    lockTypeMap(m.name)
      .filter( l => l._2 == Specific)
      .keys
      .foreach(l => {
        topLevelReserveModeMap += (l -> mkImplies(ctx.mkTrue(), ctx.mkEq(lockReserveMode, ctx.mkInt(READ))))
        topLevelReleaseModeMap += (l -> mkImplies(ctx.mkTrue(), ctx.mkEq(lockReleaseMode, ctx.mkInt(READ))))
        SMTReserveModeListMap += (l -> Set())
        SMTReleaseModeListMap += (l -> Set())
      })
    currentMod = m.name
    val nenv = lockMap(m.name).foldLeft[Environment[LockArg, Z3AST]](env)((e, mem) => e.add(mem, makeEquals(mem, Free)))
    val finalenv = checkCommand(m.body, nenv)
    //At end of execution all locks must be free or released
    finalenv.getMappedKeys().foreach(id => {
      checkState(id, finalenv, Released.order, Free.order) match {
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
          val notCurrentCond = ctx.mkNot(currentCond.asInstanceOf[Z3BoolExpr])
          if (runningPredicates.isEmpty) {
            predicates.push(currentCond)
            runningPredicates.push(notCurrentCond)
          } else {
            val runningNot = runningPredicates.pop()
            //need to add the current condition and the running Not of the previous cases to the predicates
            predicates.push(mkAnd(runningNot, currentCond))
            //add to the current running not
            runningPredicates.push(mkAnd(runningNot, notCurrentCond))
          }
          val newEnv = checkCommand(caseObj.body, env)

          //makes new environment with all locks implied by this case
          val tenv = ConditionalLockEnv(newEnv.getMappedKeys()
            .foldLeft[Map[LockArg, Z3AST]](Map())((nenv, id) => nenv + (id -> mkImplies(mkAnd(predicates.toSeq: _*), newEnv(id)))),
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
          .foldLeft[Map[LockArg, Z3AST]](Map())((nenv, id) => nenv + (id -> mkImplies(mkAnd(predicates.toSeq: _*), defEnv(id)))),
          ctx)
        predicates.pop()
        tenv.intersect(runningEnv)

      case CIf(expr, cons, alt) =>
        predicateGenerator.abstractInterpExpr(expr) match {
          case Some(value) => predicates.push(value);
          case None => predicates.push(ctx.mkEq(ctx.mkBoolConst("__TOPCONSTANT__" + incrementer), ctx.mkTrue()))
        }
        incrementer += 1

        val lt = checkCommand(cons, env)
        //makes new environment with all locks implied by true branch
        val tenv = ConditionalLockEnv(lt.getMappedKeys()
          .foldLeft[Map[LockArg, Z3AST]](Map())((nenv, id) => nenv + (id -> mkImplies(mkAnd(predicates.toSeq: _*), lt(id)))),
          ctx)
        val trueBranch = predicates.pop()

        predicates.push(ctx.mkNot(trueBranch.asInstanceOf[Z3BoolExpr]))
        val lf = checkCommand(alt, env)
        //makes new environment with all locks implied by false branch
        val fenv = ConditionalLockEnv(lf.getMappedKeys()
          .foldLeft[Map[LockArg, Z3AST]](Map())((nenv, id) => nenv + (id -> mkImplies(mkAnd(predicates.toSeq: _*), lf(id)))),
          ctx)
        predicates.pop()

        //Merge the two envs
        tenv.intersect(fenv) //real merge logic lives inside Envrionments.Z3AST

      case _: CSpeculate =>
        //TODO
        env
      case CAssign(_, rhs) => checkExpr(rhs, env)
      case CRecv(lhs, rhs) => (lhs, rhs) match {
        case (EMemAccess(mem, expr), _) =>
          checkAcquired(mem, expr, env)
        case (_, EMemAccess(mem, expr)) =>
          checkAcquired(mem, expr, env)
        case (_, ECall(mod, args)) =>
          args.foldLeft(env)((e, a) => checkExpr(a, e))
        case _ => throw UnexpectedCase(c.pos)
      }
      case c@CLockOp(mem, op, _) =>
        checkReadWriteOrder(c)
        val expectedLockState = op match {
          case Locks.Free => throw new IllegalStateException() // TODO: is this right?
          case Locks.Reserved => Free
          case Locks.Acquired => Reserved
          case Locks.Released => Acquired
        }
        checkState(mem, env, expectedLockState.order) match {
          case Z3Status.UNSATISFIABLE =>
            env.add(mem, mkImplies(mkAnd(predicates.toSeq: _*), makeEquals(mem, op)))
          case Z3Status.UNKNOWN =>
            throw new RuntimeException("An error occurred while attempting to solve the constraints")
          case Z3Status.SATISFIABLE =>
            throw new RuntimeException(s"A possible thread of execution can cause this to fail: memories needs to be $expectedLockState before $op")
        }


      case _ => env
    }
  }
  private def checkExpr(e: Expr, env: Environment[LockArg, Z3AST]): Environment[LockArg, Z3AST] = e match {
    case EUop(_, ex) => checkExpr(ex, env)
    case EBinop(_, e1, e2) =>
      val env1 = checkExpr(e1, env)
      checkExpr(e2, env1)
    case EMemAccess(mem, index) => checkAcquired(mem, index, env)
    case ETernary(cond, tval, fval) =>
      val env1 = checkExpr(cond, env)
      val env2 = checkExpr(tval, env1)
      checkExpr(fval, env2)
    case EApp(_, args) => args.foldLeft(env)((e, a) => checkExpr(a, e))
    case ECall(_, args) => args.foldLeft(env)((e, a) => checkExpr(a, e))
    case ECast(_, exp) => checkExpr(exp, env)
    case _ => env
  }
  override def checkCircuit(c: Circuit, env: Environment[LockArg, Z3AST]): Environment[LockArg, Z3AST] = env

  private def checkReadWriteOrder(c: CLockOp): Unit = {
    (c.op, c.lockType) match {
      case (Reserved, Some(LockWrite)) =>
        if (predicates.size == 1) {
          topLevelReserveModeMap += (c.mem.id -> mkImplies(ctx.mkTrue(), ctx.mkEq(lockReserveMode, ctx.mkInt(WRITE))))
        } else {
          SMTReserveModeListMap = updateSetMap(
            SMTReserveModeListMap,
            c.mem.id,
            mkImplies(mkAnd(predicates.toSeq: _*), ctx.mkEq(lockReserveMode, ctx.mkInt(WRITE))))
        }

      case (Released, Some(LockWrite)) =>
        if (predicates.size == 1) {
          topLevelReleaseModeMap += (c.mem.id -> mkImplies(ctx.mkTrue(), ctx.mkEq(lockReleaseMode, ctx.mkInt(WRITE))))
        } else {
          SMTReleaseModeListMap = updateSetMap(
            SMTReleaseModeListMap,
            c.mem.id,
            mkImplies(mkAnd(predicates.toSeq: _*), ctx.mkEq(lockReleaseMode, ctx.mkInt(WRITE))))
        }
      case (r@(Reserved | Released), Some(LockRead)) => checkLockWrite(r, c.mem.id) match {
        case Z3Status.UNSATISFIABLE =>
        case Z3Status.UNKNOWN => throw new RuntimeException("An error occurred while attempting to solve the constraints")
        case Z3Status.SATISFIABLE => throw new RuntimeException("Read type locks must be reserved or released before all write type locks " + c.pos)
      }
      case _ =>

    }
  }

  private def checkLockWrite(ls: LockState, mem: Id): Z3Status = {
    solver.add(ctx.mkEq(mkAnd(predicates.toSeq: _*), ctx.mkTrue()))
    val expectedName = ls match {
      case Released => lockReleaseMode
      case Reserved => lockReserveMode
    }
    val assertion = ls match
      {
        case Released => ctx.mkAnd((SMTReleaseModeListMap(mem) + topLevelReleaseModeMap(mem)).toSeq: _*)
        case Reserved => ctx.mkAnd((SMTReserveModeListMap(mem) + topLevelReserveModeMap(mem)).toSeq: _*)
      }
    solver.add(mkAnd(assertion, ctx.mkEq(expectedName, ctx.mkInt(WRITE))))
    //If satisfiable, this means that the lock mode is in the wrong state.
    //This is because we only call this method when checking that
    //The locks are still in Read mode, so if it is possible to be in
    //Write mode, it is error.
    val check = solver.check()
    solver.reset
    check
  }

  private def checkState(mem: LockArg, env: Environment[LockArg, Z3AST], lockStateOrders: Int*): Z3Status = {

  // Makes an OR of all given lock states
    val stateAST = lockStateOrders.foldLeft(ctx.mkFalse())((ast, order) =>
      ctx.mkOr(ast, ctx.mkEq(ctx.mkIntConst(constructVarName(mem)), ctx.mkInt(order))))
    
    // Makes all the current predicates true
    solver.add(ctx.mkEq(mkAnd(predicates.toSeq: _*), ctx.mkTrue()))
    
    // Asserts the state of the lock currently, and checks if its possible for the mem to NOT be in the expected lock states
    solver.add(mkAnd(env(mem), ctx.mkNot(stateAST)))
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
  
  private def checkAcquired(mem: Id, expr: Expr, env: Environment[LockArg, Z3AST]): Environment[LockArg, Z3AST] = {
    if (lockTypeMap(currentMod)(mem).equals(Specific) && !expr.isInstanceOf[EVar]) {
      throw new RuntimeException("We expect the argument in the memory access to be a variable")
    }
    checkState(if (lockTypeMap(currentMod)(mem).equals(General)) LockArg(mem, None) else LockArg(mem, Some(expr.asInstanceOf[EVar])),
      env, 
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

  /** Like [[Z3Context.mkAnd]], but automatically casts inputs to [[Z3BoolExpr]]s. */
  private def mkAnd(expressions: Z3AST *): Z3BoolExpr =
    ctx.mkAnd(expressions.map(ast => ast.asInstanceOf[Z3BoolExpr]):_*)

  /** Like [[Z3Context.mkImplies]], but automatically casts inputs to [[Z3BoolExpr]]s. */
  private def mkImplies(t1: Z3AST, t2: Z3AST): Z3BoolExpr =
    ctx.mkImplies(t1.asInstanceOf[Z3BoolExpr], t2.asInstanceOf[Z3BoolExpr])
}
