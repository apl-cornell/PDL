package pipedsl.typechecker

import pipedsl.common.Errors.{IllegalLockAcquisition, InvalidLockState, UnexpectedCase}
import pipedsl.common.Locks._
import pipedsl.common.{Locks, Syntax}
import pipedsl.common.Syntax._
import pipedsl.typechecker.Environments._
import pipedsl.typechecker.TypeChecker.TypeChecks

/**
 * This checks that all lock reservations happen only within a valid lock region.
 * Lock regions cannot escape the scope in which they were created (i.e. conditional branch)
 * and they can only be created for a given lock within one conditional branch.
 * - Checks: That all lock "reserve" statements occur inside the appropriate lock region.
 * - Checks: That all lock regions are well formed according to the above.
 */
object LockRegionChecker extends TypeChecks[Id, LockState] {

  override def emptyEnv(): Environment[Id, LockState] = Environments.EmptyLockEnv

  override def checkExt(e: ExternDef,
    env: Environments.Environment[Id, LockState]): Environments.Environment[Id, LockState] = env
  //Functions can't interact with locks or memories right now.
  //Could add that to the function types explicitly to be able to check applications
  override def checkFunc(f: FuncDef, env: Environment[Id, LockState]): Environment[Id, LockState] = env

  override def checkModule(m: ModuleDef, env: Environment[Id, LockState]): Environment[Id, LockState] = {
    val nenv = m.modules.foldLeft[Environment[Id, LockState]](env)( (e, m) => m.typ match {
      case TLockedMemType(_, _, _) => e.add(m.name, Free)
      case TMemType(_, _, _,_ ,_ ,_) => e.add(m.name, Free)
        //TODO eventually do need locks here
      case TModType(_, _, _, _) => e  //no locks for modules , but they're an expected type
      case TObject(_, _, _) => e //no locks here either
      case _ => throw UnexpectedCase(m.pos)
    })
    val finalStates: Environment[Id, LockState] = checkLockRegions(m.body, nenv)
    finalStates.getMappedKeys().foreach(m => finalStates(m) match {
      case Locks.Reserved | Locks.Acquired => throw InvalidLockState(m.pos, m.v, finalStates(m), Locks.Released)
      case _ => ()
    })
    env //no change to lock map after checking module
  }

  def checkLockRegions(c: Command, env: Environment[Id, LockState]): Environment[Id, LockState] = c match {
    case CSeq(c1, c2) =>
      val e1 = checkLockRegions(c1, env)
      checkLockRegions(c2, e1)
    case CTBar(c1, c2) => val e1 = checkLockRegions(c1, env)
      checkLockRegions(c2, e1)
    case CIf(_, cons, alt) =>
      val lt = checkLockRegions(cons, env)
      val lf = checkLockRegions(alt, env)
      val envfree = env.filter(Free)
      val ltfree = lt.filter(Free)
      //All locks that were Free before T branch but aren't anymore
      val ltacq = envfree -- ltfree.getMappedKeys()
      val lffree = lf.filter(Free)
      //All locks that were Free before F branch but aren't anymore
      val lfacq = envfree -- lffree.getMappedKeys()
      //If any locks were newly acquired/reserved in both branches, error
      if (ltacq.getMappedKeys().intersect(lfacq.getMappedKeys()).nonEmpty) {
        throw IllegalLockAcquisition(c.pos)
      }
      //Merge matching states, merge Free|Released states to Released, error others
      lt.intersect(lf) //real merge logic lives inside Envrionments.LockState
    case CSplit(cases, default) =>
      val caseEnvList = (default :: cases.map(c => c.body)).map(c => checkLockRegions(c, env))
      for (i <- caseEnvList.indices) {
        for (j <- caseEnvList.indices) {
          if (i != j) {
            val l1 = caseEnvList(i)
            val l2 = caseEnvList(j)
            val envfree = env.filter(Free)
            val l1free = l1.filter(Free)
            val l1acq = envfree -- l1free.getMappedKeys()
            val l2free = l2.filter(Free)
            val l2acq = envfree -- l2free.getMappedKeys()
            //If any locks were newly acquired/reserved in any two branches, error
            if (l1acq.getMappedKeys().intersect(l2acq.getMappedKeys()).nonEmpty) {
              throw IllegalLockAcquisition(c.pos)
            }
          }
        }
      }
      caseEnvList.foldLeft[Environment[Id, LockState]](caseEnvList(0))((env1, env2) => env1.intersect(env2))
    case CLockStart(mod) => env.add(mod, Acquired)
    case CLockEnd(mod) => env.add(mod, Released)
      //can only reserve locks insisde of the relevant lock region
      //other lock ops can be outside of this pass
    case CLockOp(mem, op, _, _, _) if op == Reserved =>
      if (env(mem.id) != Acquired) {
        throw InvalidLockState(c.pos, mem.id.v, env(mem.id), Acquired)
      }
      env
    case CAssign(lhs, rhs) => checkMemAccess(lhs, env); checkMemAccess(rhs, env); env
    case CRecv(lhs, rhs) => checkMemAccess(lhs, env); checkMemAccess(rhs, env); env
    case Syntax.CEmpty() => env
    case _ => env
  }

  //Check that unlocked memory accesses happen _inside_ lock regions for unlocked memories
  private def checkMemAccess(e: Expr, env: Environment[Id, LockState], isAtomic :Boolean = false): Unit = e match {
    case EIsValid(ex) => checkMemAccess(ex, env)
    case EFromMaybe(ex) => checkMemAccess(ex, env)
    case EToMaybe(ex) => checkMemAccess(ex, env)
    case EUop(_, ex) => checkMemAccess(ex, env)
    case EBinop(_, e1, e2) => checkMemAccess(e1, env); checkMemAccess(e2, env)
    case EMemAccess(mem, _, _, _, _, _) =>
      mem.typ.get match {
        case TMemType(_,_,_,_,_,_) => //only check _unlocked_ memories
          if (env(mem) != Acquired) {
            throw InvalidLockState(mem.pos, mem.v, env(mem), Acquired)
          }
        case _ if isAtomic => //also check atomic operations
          {
            if(env(mem) != Acquired) {
              throw InvalidLockState(mem.pos, mem.v, env(mem), Acquired)
            }
          }
        case _ => ()
      }
    case EBitExtract(num, _, _) => checkMemAccess(num, env)
    case ETernary(cond, tval, fval) => checkMemAccess(cond, env); checkMemAccess(tval, env); checkMemAccess(fval, env)
    case EApp(_, args) => args.foreach(a => checkMemAccess(a, env))
    case ECall(_, _, args) => args.foreach(a => checkMemAccess(a, env))
    case ECast(_, exp) => checkMemAccess(exp, env)
    case _ => ()
  }


  override def checkCircuit(c: Circuit, env: Environment[Id, LockState]): Environment[Id, LockState] = env
}
