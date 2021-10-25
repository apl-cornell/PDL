package pipedsl.typechecker

import com.microsoft.z3.{AST => Z3AST, BoolExpr => Z3BoolExpr, Context => Z3Context, Solver => Z3Solver, Status => Z3Status}
import TypeChecker._
import Environments._
import pipedsl.common.Errors.{AlreadyResolvedSpeculation, IllegalSpeculativeOperation, MismatchedSpeculationState, UnresolvedSpeculation}
import pipedsl.common.Syntax._
import pipedsl.common.Locks.{Released}
import pipedsl.common.Utilities.{mkAnd, mkImplies}

class SpeculationChecker(val ctx: Z3Context) extends TypeChecks[Id, Z3AST] {

  object SpecState extends Enumeration {
    type SpecState = Value
    val Unknown, Speculative, NonSpeculative = Value
  }

  import SpecState._

  private val solver: Z3Solver = ctx.mkSolver()

  //TODO should pass this as recursive argument instead, but it's OK
  private var memsWithCheckpoints: Set[Id] = Set()

  override def emptyEnv(): Environment[Id, Z3AST] = ConditionalEnv(ctx = ctx)

  override def checkExt(e: ExternDef, env: Environment[Id, Z3AST]): Environment[Id, Z3AST] = env
  override def checkFunc(f: FuncDef, env: Environment[Id, Z3AST]): Environment[Id, Z3AST] = env
  override def checkCircuit(c: Circuit, env: Environment[Id, Z3AST]): Environment[Id, Z3AST] = env

  type Status = Int
  private val INIT = 0
  private val STARTED = 1
  private val RESOLVED = 2

  override def checkModule(m: ModuleDef, env: Environment[Id, Z3AST]): Environment[Id, Z3AST] = {
    //reset state (don't call this concurrently)
    memsWithCheckpoints = Set()
    //end reset
    if (m.maybeSpec) {
      val finalSpec = checkSpecOps(m.body, Unknown)
      if (finalSpec != NonSpeculative) throw UnresolvedSpeculation(m.pos, m.name)
      val specIds = getSpecIds(m.body, Set())
      val startEnv = specIds.foldLeft(env)((e, id) => {
        if (e.get(id).isEmpty) {
          e.add(id, makeEquals(id, state = INIT))
        } else { e }
      })
      val finalEnv = checkResolved(m.body, startEnv)
      //check that all spechandles are definitely resolved or never started
      finalEnv.getMappedKeys().foreach(k => {
        checkPossibleStates(k, finalEnv, ctx.mkTrue(), expected = RESOLVED,INIT)
      })
    }
    //Once checking all of the speculation, now check checkpoints:
    checkCheckpoints(m.body)
    env
  }

  /**
   * Speculation for the current thread must be completely resolved (via a blocking speccheck)
   * before any of the following commands are executed
   *  - Releasing any Write locks
   *  - Unlocked Memory Writes
   *  - Updating the results of speculation made by this thread (CVerify)
   *
   *  The current stage must have a non-blocking spec check to run the following commands:
   *  - Lock operations
   *  - Speculative Calls
   *  - Killing child threads (CInvalidate, CUpdate)
   *  - Locked Memory Writes (Reads??)
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
        //must be nonspeculative if unlocked, else must be at least not definitely misspeculated
      case EMemAccess(m, _, _ ,_ ,_, _) =>
        if (s != NonSpeculative && !isLockedMemory(m)) {
          throw IllegalSpeculativeOperation(lhs.pos, NonSpeculative.toString)
        }
        if (s == Unknown && isLockedMemory(m)) {
          throw IllegalSpeculativeOperation(lhs.pos, Speculative.toString)
        }
        ()
      case _ => ()
    }; s
    case CSpecCall(_, _, _) if s == Unknown =>
      throw IllegalSpeculativeOperation(c.pos, Speculative.toString)
    case CCheckSpec(isBlocking) => if (s != Unknown)  throw IllegalSpeculativeOperation(c.pos, Unknown.toString)
      if (isBlocking) { NonSpeculative }
      else { Speculative }
    case CVerify(_, _, _,_, _) if s != NonSpeculative =>
      throw IllegalSpeculativeOperation(c.pos, NonSpeculative.toString)
    case CUpdate(_, _, _, _, _) if s == Unknown =>
      throw IllegalSpeculativeOperation(c.pos, Speculative.toString)
    case CInvalidate(_, _) => s // can always invalidate speculation
    case COutput(_) if s != NonSpeculative =>
      throw IllegalSpeculativeOperation(c.pos, NonSpeculative.toString)
    case CLockStart(_) => s //this should be OK to do speculatively or not
    case CLockEnd(_) => s
    case CCheckpoint(_, lock) => //mark lock as having a checkpoint
      memsWithCheckpoints = memsWithCheckpoints + lock
      s
      //can only release READ-ONLY locks speculatively
      //any lock that _might_ allow writes needs to be Non-Speculatively
    case CLockOp(mem, op, t, _, _) => {
      if (s != NonSpeculative && !memsWithCheckpoints.contains(mem.id)) {
        //TODO error that actually says something about checkpoints
        throw IllegalSpeculativeOperation(c.pos, NonSpeculative.toString)
      }
      if (op == Released && (t.isEmpty || t.get == LockWrite) && s != NonSpeculative) {
        throw IllegalSpeculativeOperation(c.pos, NonSpeculative.toString)
      }
      //shouldn't do any potentially speculative lock ops w/o checking first
      if (s == Unknown) {
        throw IllegalSpeculativeOperation(c.pos, Speculative.toString)
      }
      s
    }
    case _ => s
  }

  //Return all of the Ids representing speculation handles produced by speccall or used by verify et al. statements
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
    case CVerify(handle, _, _, _, _) =>
      env + handle.id
    case CUpdate(nh, handle, _, _, _) =>
      env + handle.id + nh.id
    case CInvalidate(handle, _) =>
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
      val lenv = mapConditionalContext(lt, cons)
      val renv = mapConditionalContext(rt, alt)
      lenv.intersect(renv)
    case CSplit(cases, default) =>
      val startEnv = mapConditionalContext(env, default)
      cases.foldLeft[Environment[Id,Z3AST]](checkResolved(default, startEnv))((menv, cobj) => {
        //merge logic lives in Environments (conjunction of branches)
        val condenv = mapConditionalContext(checkResolved(cobj.body, env), cobj.body)
        menv.intersect(condenv)
      })
    case CSpecCall(handle, _, _) =>
      checkPossibleStates(handle.id, env, c.predicateCtx.get, expected = INIT)
      env.add(handle.id,makeEquals(handle.id, state = STARTED))
    case CVerify(handle, _, _, _, _) =>
      checkPossibleStates(handle.id, env, c.predicateCtx.get, expected = STARTED)
      env.add(handle.id, makeEquals(handle.id, state = RESOLVED))
    case CUpdate(nh, handle, _, _, _) =>
      //"reolves" the current spec but starts a new one
      checkPossibleStates(handle.id, env, c.predicateCtx.get, expected = STARTED)
      checkPossibleStates(nh.id, env, c.predicateCtx.get, expected = INIT)
      env.add(handle.id, makeEquals(handle.id, state = RESOLVED)).add(
        nh.id, makeEquals(nh.id, state = STARTED)
      )
    case CInvalidate(handle, _) =>
      checkPossibleStates(handle.id, env, c.predicateCtx.get, expected = STARTED)
      env.add(handle.id, makeEquals(handle.id, state = RESOLVED))
    case _ => env
  }

  private def mapConditionalContext(lt: Environment[Id, Z3AST], c: Command): Environment[Id, Z3AST] = {
    lt.map(n => { mkImplies(ctx, c.predicateCtx.get, n) }, emptyEnv())
  }

  private def makeEquals(specId: Id, state: Status): Z3BoolExpr = {
    ctx.mkEq(ctx.mkIntConst(specId.v), ctx.mkInt(state))
  }

  //TODO make error messages talk about the right thing (checkpoints vs. speculation)
  private def checkPossibleStates(specId: Id, env: Environment[Id, Z3AST], preds: Z3BoolExpr, expected: Status*): Unit = {
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
          throw UnresolvedSpeculation(specId.pos, specId)
        } else {
          throw AlreadyResolvedSpeculation(specId.pos, specId)
        }
      case Z3Status.UNKNOWN =>
        throw new RuntimeException("An error occurred while attempting to check speculation resolution")
      case Z3Status.UNSATISFIABLE => ()
    }
  }

  /**
   * This class checks that checkpoints are instantiated in the correct context.
   * - If a pipeline may speculatively reserve locks, then it must have a checkpoint for that memory
   *   (computed and stored in memsWithCheckpoints).
   * - Checkpoints must be instantiated in the context in which they are used (i.e., with the necessary branch conditions)
   * - Checkpoints must be resolved by some Verify or Invalidate
   */
  private def checkCheckpoints(c: Command): Unit = {
    val startEnv = getCheckIds(c, Set()).foldLeft(emptyEnv())((e, id) => {
      e.add(id, makeEquals(id, state = INIT))
    })
    val endEnv = checkCheckpointsHelper(c, startEnv)
    //check that all necessary checkpoints are resolved
    endEnv.getMappedKeys().foreach(k => {
      checkPossibleStates(k, endEnv, ctx.mkTrue(), expected = RESOLVED, INIT)
    })
    ()
  }

  private def checkCheckpointsHelper(c: Command, env: Environment[Id, Z3AST]): Environment[Id, Z3AST] = c match {
    case CSeq(c1, c2) => checkCheckpointsHelper(c2, checkCheckpointsHelper(c1, env))
    case CTBar(c1, c2) => checkCheckpointsHelper(c2, checkCheckpointsHelper(c1, env))
    case CIf(_, cons, alt) =>
      val lt = mapConditionalContext(checkCheckpointsHelper(cons, env), cons)
      val rt = mapConditionalContext(checkCheckpointsHelper(alt, env), alt)
      lt.intersect(rt)
    case CSplit(cases, default) =>
      cases.foldLeft[Environment[Id,Z3AST]](checkCheckpointsHelper(default,
        mapConditionalContext(env, default)))((menv, cobj) => {
        //merge logic lives in Environments (conjunction of branches)
        menv.intersect(checkCheckpointsHelper(cobj.body, env))
      })
    case CVerify(_, _, _, _, checkHandles) =>
      checkHandles.foldLeft(env)((e, chk) => {
        //check that checkpoint has been made in this context
        checkPossibleStates(chk.id, env, c.predicateCtx.get, expected = STARTED)
        //add implication that chk is resolved
        e.add(chk.id, makeEquals(chk.id, state = RESOLVED))
      })
    case CUpdate(_, _, _, _, checkHandles) =>
      checkHandles.foldLeft(env)((e, chk) => {
        //check that checkpoint has been made in this context
        checkPossibleStates(chk.id, env, c.predicateCtx.get, expected = STARTED)
        e
      })
    case CInvalidate(_, checkHandles) =>
      checkHandles.foldLeft(env)((e, chk) => {
        //check that checkpoint has been made in this context
        checkPossibleStates(chk.id, env, c.predicateCtx.get, expected = STARTED)
        //add implication that chk is resolved
        e.add(chk.id, makeEquals(chk.id, state = RESOLVED))
      })
    case CCheckpoint(handle, _) => //set the checkpoint state to initialized in this context
      env.add(handle.id, makeEquals(handle.id, state = STARTED))
    case _ => env
  }

  //Return all of the Ids representing checkpoint handles used in the program
  private def getCheckIds(c: Command, env: Set[Id] ): Set[Id] = c match {
    case CSeq(c1, c2) => getCheckIds(c2, getCheckIds(c1, env))
    case CTBar(c1, c2) => getCheckIds(c2, getCheckIds(c1, env))
    case CIf(_, cons, alt) =>
      getCheckIds(cons, env) ++ getCheckIds(alt, env)
    case CSplit(cases, default) =>
      cases.foldLeft[Set[Id]](getCheckIds(default, env))((menv, cobj) => {
        getCheckIds(cobj.body, menv)
      })
    case CCheckpoint(handle, _) => env + handle.id
    case _ => env
  }

}
