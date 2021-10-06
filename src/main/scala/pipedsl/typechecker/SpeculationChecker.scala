package pipedsl.typechecker

import com.microsoft.z3.{AST => Z3AST, BoolExpr => Z3BoolExpr, Context => Z3Context, Solver => Z3Solver, Status => Z3Status}
import TypeChecker._
import Environments._
import pipedsl.common.Errors.{AlreadyResolvedSpeculation, IllegalSpeculativeOperation, MismatchedSpeculationState, UnresolvedSpeculation}
import pipedsl.common.Syntax._
import pipedsl.common.Locks.{Released, Reserved}
import pipedsl.common.Utilities.{mkAnd, mkImplies}

class SpeculationChecker(val ctx: Z3Context) extends TypeChecks[Id, Z3AST] {

  object SpecState extends Enumeration {
    type SpecState = Value
    val Unknown, Speculative, NonSpeculative = Value
  }

  import SpecState._

  private val solver: Z3Solver = ctx.mkSolver()

  override def emptyEnv(): Environment[Id, Z3AST] = ConditionalEnv(ctx = ctx)

  override def checkExt(e: ExternDef, env: Environment[Id, Z3AST]): Environment[Id, Z3AST] = env

  //No Speculation in Functions
  override def checkFunc(f: FuncDef, env: Environment[Id, Z3AST]): Environment[Id, Z3AST] = env

  type Status = Int;
  private val INIT = 0
  private val STARTED = 1
  private val RESOLVED = 2

  override def checkModule(m: ModuleDef, env: Environment[Id, Z3AST]): Environment[Id, Z3AST] = {
    if (m.maybeSpec) {
      val finalSpec = checkSpecOps(m.body, Unknown)
      if (finalSpec != NonSpeculative) throw UnresolvedSpeculation(m.pos)
      val specIds = getSpecIds(m.body, Set())
      val startEnv = specIds.foldLeft(env)((e, id) => {
        if (e.get(id).isEmpty) {
          e.add(id, makeEquals(id, state = INIT))
        } else { e }
      })
      val finalEnv = checkResolved(m.body, startEnv)
      //check that all spechandles are definitely resolved
      finalEnv.getMappedKeys().foreach(k => {
        checkResolved(k, finalEnv, ctx.mkTrue(), expected = RESOLVED,INIT)
      })
    }
    env
  }

  /**
   * Speculation for the current thread must be completely resolved (via a blocking speccheck)
   * before any of the following commands are executed:
   *  - Writes to memories (CRECV with memories on LHS)
   *  (TODO we can move this into nb. check once we update lock libraries)
   *  - Releasing any Write locks
   *  - Updating the results of speculation made by this thread (CVerify)
   *
   *  The current stage must have a non-blocking spec check to run the following commands:
   *  - Lock reservation
   *  - Speculative Calls
   *
   * @param c The command to check
   * @param s The speculative state of this thread at command c
   * @return The new speculative state of this thread after command c
   */
  def checkSpecOps(c: Command, s: SpecState): SpecState = c match {
    case CSeq(c1, c2) => checkSpecOps(c2, checkSpecOps(c1, s))
    case CTBar(c1, c2) => checkSpecOps(c1, s) match {
        case NonSpeculative => checkSpecOps(c2, NonSpeculative)
        case _ => checkSpecOps(c2, Unknown)
    }
    case CIf(_, cons, alt) => //make sure all branches agree
      val strue = checkSpecOps(cons, s)
      val sfalse = checkSpecOps(alt, s)
      if (strue != sfalse) { throw MismatchedSpeculationState(c.pos) }
      else strue
    case CSplit(cases, default) => //make sure all branches agree
      val sdef = checkSpecOps(default, s)
      cases.foreach(co => {
        val sco = checkSpecOps(co.body, s)
        if (sco != sdef) { throw MismatchedSpeculationState(co.body.pos) }
      })
      sdef
    case CRecv(lhs, _) => lhs match {
        //just match on mem writes
      case _ :EMemAccess => if (s != NonSpeculative)
        throw IllegalSpeculativeOperation(lhs.pos, NonSpeculative.toString)
      case _ => ()
    }; s
    case CSpecCall(_, _, _) if s == Unknown =>
      throw IllegalSpeculativeOperation(c.pos, Speculative.toString)
    case CCheckSpec(isBlocking) => if (s != Unknown)  throw IllegalSpeculativeOperation(c.pos, Unknown.toString)
      if (isBlocking) { NonSpeculative }
      else { Speculative }
    case CVerify(_, _, _,_) if s != NonSpeculative =>
      throw IllegalSpeculativeOperation(c.pos, NonSpeculative.toString)
    case CUpdate(_, _, _, _) if s == Unknown =>
      throw IllegalSpeculativeOperation(c.pos, Speculative.toString)
    case CInvalidate(_) => s // can always invalidate speculation
    case COutput(_) if s != NonSpeculative =>
      throw IllegalSpeculativeOperation(c.pos, NonSpeculative.toString)
    case CLockStart(_) => s //this should be OK to do speculatively or not
    case CLockEnd(_) => s //same
      //can only release READ-ONLY locks non-speculatively
    case CLockOp(_, op, t, _, _) if op == Released && (t.isEmpty || t.get == LockWrite ) && s != NonSpeculative =>
      throw IllegalSpeculativeOperation(c.pos, NonSpeculative.toString)
    case CLockOp(_, op, _, _, _)  if op == Reserved && s == Unknown=>
      throw IllegalSpeculativeOperation(c.pos, Speculative.toString)
    case _ => s
  }

  private def getSpecIds(c: Command, env: Set[Id] ): Set[Id] = c match {
    case CSeq(c1, c2) => getSpecIds(c2, getSpecIds(c1, env))
    case CTBar(c1, c2) => getSpecIds(c2, getSpecIds(c1, env))
    case CIf(_, cons, alt) =>
      getSpecIds(cons, env) ++ getSpecIds(alt, env)
    case CSplit(cases, default) =>
      cases.foldLeft[Set[Id]](getSpecIds(default, env))((menv, cobj) => {
        getSpecIds(cobj.body, menv)
      })
    case CSpecCall(handle, _, _) =>
      env + handle.id
    case CVerify(handle, _, _, _) =>
      env + handle.id
    case CUpdate(nh, handle, _, _) =>
      env + handle.id + nh.id
    case CInvalidate(handle) =>
      env + handle.id
    case _ => env
  }
  /**
   * This checks whether or not a parent that creates speculative events
   * actually resolves them later or not. And it must resolve them exactly once
   * on all possible execution paths.
   * @param c The Command to check
   * @param env The context of which speculation handles have or haven't been resolved
   *            and under which program predicates that is the case.
   * @return A new context of speculation handle states.
   */
  def checkResolved(c: Command, env: Environment[Id, Z3AST]): Environment[Id, Z3AST] = c match {
    case CSeq(c1, c2) => checkResolved(c2, checkResolved(c1, env))
    case CTBar(c1, c2) => checkResolved(c2, checkResolved(c1, env))
    case CIf(_, cons, alt) =>
      val lt = checkResolved(cons, env)
      val rt = checkResolved(alt, env)
      lt.intersect(rt)
    case CSplit(cases, default) =>
      cases.foldLeft[Environment[Id,Z3AST]](checkResolved(default, env))((menv, cobj) => {
        //merge logic lives in Environments (conjunction of branches)
        menv.intersect(checkResolved(cobj.body, env))
      })
    case CSpecCall(handle, _, _) =>
      checkResolved(handle.id, env, c.predicateCtx.get, expected = INIT)
      env.add(handle.id,
      mkImplies(ctx, c.predicateCtx.get, makeEquals(handle.id, state = STARTED)))
    case CVerify(handle, _, _, _) =>
      checkResolved(handle.id, env, c.predicateCtx.get, expected = STARTED)
      env.add(handle.id, mkImplies(ctx, c.predicateCtx.get, makeEquals(handle.id, state = RESOLVED)))
    case CUpdate(nh, handle, _, _) =>
      //"reolves" the current spec but starts a new one
      checkResolved(handle.id, env, c.predicateCtx.get, expected = STARTED)
      checkResolved(nh.id, env, c.predicateCtx.get, expected = INIT)
      env.add(handle.id, mkImplies(ctx, c.predicateCtx.get, makeEquals(handle.id, state = RESOLVED))).add(
        nh.id, mkImplies(ctx, c.predicateCtx.get, makeEquals(nh.id, state = STARTED))
      )
    case CInvalidate(handle) =>
      checkResolved(handle.id, env, c.predicateCtx.get, expected = STARTED)
      env.add(handle.id, mkImplies(ctx, c.predicateCtx.get, makeEquals(handle.id, state = RESOLVED)))
    case _ => env
  }

  override def checkCircuit(c: Circuit, env: Environment[Id, Z3AST]): Environment[Id, Z3AST] = env

  private def makeEquals(specId: Id, state: Status): Z3BoolExpr = {
    ctx.mkEq(ctx.mkIntConst(specId.v), ctx.mkInt(state))
  }

  private def checkResolved(specId: Id, env: Environment[Id, Z3AST], preds: Z3BoolExpr, expected: Status*): Unit = {
    //Prove that the UNexpected, CAN'T happen
    val expectedStates = expected.foldLeft(ctx.mkFalse())((ast, e) => {
      ctx.mkOr(ast, makeEquals(specId, e))
    })
    val expResolved = ctx.mkNot(expectedStates)
    //Assume current context predicates
    solver.add(ctx.mkEq(preds, ctx.mkTrue()))
    //Assert current state of the speculation based on env and what we expect not to happen
    solver.add(mkAnd(ctx, env(specId), expResolved))
    val check = solver.check()
    solver.reset()
    check match {
        //error! bad thing can happen
      case Z3Status.SATISFIABLE =>
        if (expected.contains(RESOLVED)) {
          throw UnresolvedSpeculation(specId.pos)
        } else {
          throw AlreadyResolvedSpeculation(specId.pos)
        }
      case Z3Status.UNKNOWN =>
        throw new RuntimeException("An error occurred while attempting to check speculation resolution")
      case Z3Status.UNSATISFIABLE => ()
    }
  }

}
