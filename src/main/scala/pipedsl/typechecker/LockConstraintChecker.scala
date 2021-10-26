package pipedsl.typechecker

import com.microsoft.z3.{AST => Z3AST, BoolExpr => Z3BoolExpr, Context => Z3Context, Solver => Z3Solver, Status => Z3Status}
import pipedsl.common.Errors.{UnexpectedCase, UnprovenLockState}
import pipedsl.common.Locks
import pipedsl.common.Locks._
import pipedsl.common.Syntax._
import pipedsl.common.Utilities.{mkAnd, mkImplies, updateSetMap}
import pipedsl.typechecker.Environments._
import pipedsl.typechecker.TypeChecker.TypeChecks
/*want to also check that writes are done precisely once*/
/*maybe keep a map from mems to Z3AST that keeps track of on what conditions there is a write?*/
/*need to cross check this with the conditions under which the lock is reserved*/
/*reserve a write lock on mem => we know under what conditions*/
/*write to mem. These conditions should not overlap with anything else which*/
/*writes to mem, and at the end we can check that all conditions under which*/
/*the lock is reserved are written in*/
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
 *
 * Don't check mem accesses for Unlocked Memories => these are only allowed inside lock regions and are checked there.
 */
//TODO: Make error case classes
class LockConstraintChecker(lockMap: Map[Id, Set[LockArg]], lockGranularityMap: Map[Id, Map[Id, LockGranularity]], val ctx: Z3Context)
  extends TypeChecks[LockArg, Z3AST] {
  private val solver: Z3Solver = ctx.mkSolver()

  private val predicates: mutable.Stack[Z3AST] = mutable.Stack(ctx.mkTrue())

  private val lockReserveMode = ctx.mkIntConst("ReservedMode")
  private val lockReleaseMode = ctx.mkIntConst("ReleasedMode")

  private var topLevelReserveModeMap: Map[Id, Z3BoolExpr] = Map()
  private var topLevelReleaseModeMap: Map[Id, Z3BoolExpr] = Map()
  private var SMTReserveModeListMap: Map[Id, Set[Z3BoolExpr]] = Map()
  private var SMTReleaseModeListMap: Map[Id, Set[Z3BoolExpr]] = Map()
  private val READ = 0
  private val WRITE = 1

  private val writeReserveMap :mutable.HashMap[Id, Z3BoolExpr] = mutable.HashMap()
  private val writeDoMap :mutable.HashMap[Id, Z3BoolExpr] = mutable.HashMap()


  private var currentMod = Id("-invalid-")

  override def emptyEnv(): Environment[LockArg, Z3AST] = ConditionalLockEnv(ctx = ctx)

  override def checkExt(e: ExternDef,
    env: Environments.Environment[LockArg, Z3AST]): Environments.Environment[LockArg, Z3AST] = env

  //Functions can't interact with locks or memories right now.
  //Could add that to the function types explicitly to be able to check applications
  override def checkFunc(f: FuncDef, env: Environment[LockArg, Z3AST]): Environment[LockArg, Z3AST] = env

  override def checkModule(m: ModuleDef, env: Environment[LockArg, Z3AST]): Environment[LockArg, Z3AST] = {
    //Reads must go before writes, so the modes are initialized to read
    lockGranularityMap(m.name)
      .filter(l => l._2 == Specific)
      .keys
      .foreach(l => {
        topLevelReserveModeMap += (l -> mkImplies(ctx, ctx.mkTrue(), ctx.mkEq(lockReserveMode, ctx.mkInt(READ))))
        topLevelReleaseModeMap += (l -> mkImplies(ctx, ctx.mkTrue(), ctx.mkEq(lockReleaseMode, ctx.mkInt(READ))))
        SMTReserveModeListMap += (l -> Set())
        SMTReleaseModeListMap += (l -> Set())
      })
    currentMod = m.name
    writeDoMap.clear(); writeReserveMap.clear()
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
    writeReserveMap.foreachEntry((mem, when_reserved) =>
      {
        solver.add(ctx.mkXor(when_reserved, writeDoMap.getOrElse(mem,
        throw new RuntimeException("Some write locks are reserved without being used"))))
        solver.check() match
        {
          case Z3Status.UNSATISFIABLE =>
          case Z3Status.UNKNOWN =>
            throw new RuntimeException("Some write locks may be reserved without being used")
          case Z3Status.SATISFIABLE =>
            throw new RuntimeException("Some write locks are reserved without being used")
        }})

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

        //TODO refactor this code so it looks like the code in the SpeculationChecker (it does _exactly_ the same
        //thing, I just think it looks nicer there)
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
      case CAssign(_, rhs) => checkExpr(rhs, env, c.predicateCtx.get)
      case CRecv(lhs, rhs) => (lhs, rhs) match {
        case (EMemAccess(mem, expr, _, _, _, isAtomic), _) =>
          /*this is a write*/
          if (isLockedMemory(mem) && !isAtomic) {
            checkDisjoint(mem, c.predicateCtx.get)
            checkAcquired(mem, expr, env, c.predicateCtx.get)
          } else {
            env
          }
        case (_, EMemAccess(mem, expr, _, _, _, isAtomic)) =>
          if (isLockedMemory(mem) && !isAtomic) {
            checkAcquired(mem, expr, env, c.predicateCtx.get)
          } else {
            env
          }
        case (_, ECall(_, _, args)) =>
          args.foldLeft(env)((e, a) => checkExpr(a, e, c.predicateCtx.get))
        case _ => throw UnexpectedCase(c.pos)
      }
      case c@CLockOp(mem, op, _, _, _) =>
        checkReadWriteOrder(c)
        val expectedLockState = op match {
          case Locks.Free => throw new IllegalStateException() // TODO: is this right?
          case Locks.Reserved =>
            if(mem.memOpType.contains(LockWrite))
              {
                writeReserveMap.update(mem.id,
                  ctx.mkOr(writeReserveMap.getOrElse(mem.id, ctx.mkFalse()),
                    c.predicateCtx.get))
              }
            Free
          case Locks.Acquired => Reserved
          case Locks.Released => Acquired
        }
        checkState(mem, env, c.predicateCtx.get, expectedLockState.order) match {
          case Z3Status.UNSATISFIABLE =>
            env.add(mem, mkImplies(ctx, c.predicateCtx.get, makeEquals(mem, op)))
          case Z3Status.UNKNOWN =>
            throw new RuntimeException("An error occurred while attempting to solve the constraints")
          case Z3Status.SATISFIABLE =>
            throw UnprovenLockState(c.pos, mem.id.v, expectedLockState)
        }
      case _ => env
    }
  }

  private def checkExpr(e: Expr, env: Environment[LockArg, Z3AST], predicates: Z3BoolExpr): Environment[LockArg, Z3AST] = e match {
    case EUop(_, ex) => checkExpr(ex, env, predicates)
    case EBinop(_, e1, e2) =>
      val env1 = checkExpr(e1, env, predicates)
      checkExpr(e2, env1, predicates)
    case EMemAccess(mem, index, _, _, _, isAtomic) if isLockedMemory(mem) && !isAtomic =>
      //TODO this throws bad error if never reserved in any context (env(mem) crashes)
      //it should fail (which is correct), but the error could be nicer
      checkAcquired(mem, index, env, predicates)
    case ETernary(cond, tval, fval) =>
      val env1 = checkExpr(cond, env, predicates)
      val env2 = checkExpr(tval, env1, predicates)
      checkExpr(fval, env2, predicates)
    case EApp(_, args) => args.foldLeft(env)((e, a) => checkExpr(a, e, predicates))
    case ECall(_, _, args) => args.foldLeft(env)((e, a) => checkExpr(a, e, predicates))
    case ECast(_, exp) => checkExpr(exp, env, predicates)
    case _ => env
  }

  override def checkCircuit(c: Circuit, env: Environment[LockArg, Z3AST]): Environment[LockArg, Z3AST] = env

  private def checkReadWriteOrder(c: CLockOp): Unit = {
    (c.op, c.lockType) match {
      case (Reserved, Some(LockWrite)) =>
        if (predicates.size == 1) {
          topLevelReserveModeMap += (c.mem.id -> mkImplies(ctx, ctx.mkTrue(), ctx.mkEq(lockReserveMode, ctx.mkInt(WRITE))))
        } else {
          SMTReserveModeListMap = updateSetMap(
            SMTReserveModeListMap,
            c.mem.id,
            mkImplies(ctx, mkAnd(ctx, predicates.toSeq: _*), ctx.mkEq(lockReserveMode, ctx.mkInt(WRITE))))
        }

      case (Released, Some(LockWrite)) =>
        if (predicates.size == 1) {
          topLevelReleaseModeMap += (c.mem.id -> mkImplies(ctx, ctx.mkTrue(), ctx.mkEq(lockReleaseMode, ctx.mkInt(WRITE))))
        } else {
          SMTReleaseModeListMap = updateSetMap(
            SMTReleaseModeListMap,
            c.mem.id,
            mkImplies(ctx, mkAnd(ctx, predicates.toSeq: _*), ctx.mkEq(lockReleaseMode, ctx.mkInt(WRITE))))
        }
      case (r@(Reserved | Released), Some(LockRead)) => checkLockWrite(r, c.mem.id) match {
        case Z3Status.UNSATISFIABLE =>
        case Z3Status.UNKNOWN => throw new RuntimeException("An error occurred while attempting to solve the constraints")
        case Z3Status.SATISFIABLE => throw new RuntimeException("Read type locks must be reserved or released before all write type locks " + c.pos)
      }
      case _ => ()
    }
  }

  private def checkLockWrite(ls: LockState, mem: Id): Z3Status = {
    solver.add(ctx.mkEq(mkAnd(ctx, predicates.toSeq: _*), ctx.mkTrue()))
    val expectedName = ls match {
      case Released => lockReleaseMode
      case Reserved => lockReserveMode
      case _ => assert(false); lockReleaseMode //TODO throw good exception
    }
    val assertion = ls match {
      case Released => ctx.mkAnd((SMTReleaseModeListMap(mem) + topLevelReleaseModeMap(mem)).toSeq: _*)
      case Reserved => ctx.mkAnd((SMTReserveModeListMap(mem) + topLevelReserveModeMap(mem)).toSeq: _*)
      case _ => assert(false); lockReleaseMode //TODO throw good exception
    }
    solver.add(mkAnd(ctx, assertion, ctx.mkEq(expectedName, ctx.mkInt(WRITE))))
    //If satisfiable, this means that the lock mode is in the wrong state.
    //This is because we only call this method when checking that
    //The locks are still in Read mode, so if it is possible to be in
    //Write mode, it is error.
    val check = solver.check()
    solver.reset()
    check
  }

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
    mem.id.v + (if (mem.evar.isDefined) "[" + mem.evar.get.id.v + "]" else "")
  }

  private def checkAcquired(mem: Id, expr: Expr, env: Environment[LockArg, Z3AST], predicates: Z3BoolExpr): Environment[LockArg, Z3AST] = {
    if (lockGranularityMap(currentMod)(mem).equals(Specific) && !expr.isInstanceOf[EVar]) {
      throw new RuntimeException("We expect the argument in the memory access to be a variable")
    }
    checkState(if (lockGranularityMap(currentMod)(mem).equals(General)) LockArg(mem, None) else LockArg(mem, Some(expr.asInstanceOf[EVar])),
      env,
      predicates,
      Acquired.order)
    match {
      case Z3Status.SATISFIABLE =>
        throw UnprovenLockState(expr.pos, mem.v, Locks.Acquired)
      case Z3Status.UNKNOWN =>
        throw new RuntimeException("An error occurred while attempting to solve the constraints")
      case Z3Status.UNSATISFIABLE =>
        env
    }
  }

  private def checkDisjoint(mem: Id, cond: Z3BoolExpr) :Unit =
    {
      /*called when we do a write. Check that no other writes are done in this case*/
      val old_expr = writeDoMap.getOrElse(mem, ctx.mkFalse())
      solver.add(ctx.mkAnd(old_expr, cond))
      solver.check() match
      {
        case Z3Status.UNSATISFIABLE =>
        case Z3Status.UNKNOWN =>
          throw new RuntimeException("It is possible that there are two write ops under the same lock")
        case Z3Status.SATISFIABLE =>
          throw new RuntimeException("There are two write ops under the same lock")
      }
      solver.reset()
      writeDoMap.update(mem, ctx.mkOr(old_expr, cond))
    }
}
