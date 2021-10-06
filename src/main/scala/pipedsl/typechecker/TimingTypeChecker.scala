package pipedsl.typechecker

import pipedsl.common.Syntax._
import TypeChecker.TypeChecks
import pipedsl.common.Errors.{MissingType, UnavailableArgUse, UnexpectedAsyncReference, UnexpectedCommand, UnexpectedType, UnsupportedLockOperation}
import pipedsl.common.{LockImplementation, Syntax}
import Environments.Environment
import pipedsl.common.Syntax.Latency.{Asynchronous, Combinational, Latency}
import pipedsl.common.Utilities.{getMemReads, is_handle_var}

/**
 * - Checks that variables set by receive statements
 *   are not used until after a `---` separator (additionally checking
 *   that any reference to a variable happens after it has been assigned).
 * - Checks that any noncombinational access to memory or an external module happens as part of a
 *   "receive" statement rather than a normal assign or other combinational expression.
 * - Ensures that no pipeline splitting operations are conducted inside an if statement
 */
object TimingTypeChecker extends TypeChecks[Id, Type] {

  type Available = Set[Id]
  val NoneAvailable: Available = Set[Id]()

  override def emptyEnv(): Environment[Id, Type] = Environments.EmptyTypeEnv

  override def checkExt(e: ExternDef, env: Environment[Id, Type]): Environment[Id, Type] = env

  //Functions are combinational, this is checked in their well-formedness check
  override def checkFunc(f: FuncDef, env: Environment[Id, Type]): Environment[Id, Type] = env

  override def checkModule(m: ModuleDef, env: Environment[Id, Type]): Environment[Id, Type] = {
    val inputs = m.inputs.foldLeft[Available](NoneAvailable)((av,p) => {
      av + p.name
    })
    val allAvailable = m.modules.foldLeft[Available](inputs)((av, m) => {
      av + m.name
    })
    checkCommand(m.body, allAvailable, NoneAvailable)
    env
  }

  /**
   * TODO comment
   * @param lhs
   * @param rhs
   * @param isRecv
   * @param vars
   * @param nextVars
   * @return
   */
  private def check_recv_or_asn(lhs :Expr, rhs :Expr, isRecv :Boolean,
                                vars: Available, nextVars: Available): (Available, Available) =
  {
    val _ = checkExpr(rhs, vars)
    val _ = checkExpr(lhs, vars, isRhs = false)

    def assignReadLocks(id: Id, e: EMemAccess, now: Available, later: Available): (Available, Available) = {
      if (e.isAtomic) {
        if (isRecv) (vars, nextVars + id) else (vars + id, nextVars)
      } else {
        val accessOp = if (isSynchronousAccess(e.mem, isWrite = false)) None else Some(LockRead)
        val outHset: Set[Id] = if (e.outHandle.isDefined) Set(e.outHandle.get.id) else Set()
        LockImplementation.getAccess(LockImplementation.getLockImpl(e), accessOp) match {
          case Some((_, Combinational)) if isRecv => (now.union(outHset), later + id)
          case Some((_, Combinational)) if !isRecv => (now.union(outHset) + id, later)
          case Some(_) if isRecv => (now, (later + id).union(outHset))
          case Some(_) if !isRecv => (now + id, later.union(outHset))
          case None => throw UnsupportedLockOperation(e.mem, "Read", e.pos)
        }
      }
    }

    (lhs, rhs) match {
      //don't need to check handles for unlocked mems
      case (EVar(id), e@EMemAccess(mem, _, _, inHandle, outHandle, isAtomic)) if isLockedMemory(mem) =>
        inHandle match
        {
          case Some(in) => checkExpr(in, vars)
          case None => ()
        }
        val outHset :Available = outHandle match {
          case Some(evar) => Set(evar.id)
          case None => Set()
        }
        assignReadLocks(id, e, vars, nextVars)
      case (EVar(id), ECall(_,_,_)) if isRecv => (vars, nextVars + id)
      case (EVar(id), ECall(mod, Some(method), args)) =>
        args.foreach(a => checkExpr(a, vars))
        mod.typ.get match {
          case TObject(_, _, methods) => methods(method)._2 match {
            case Combinational => (vars + id, nextVars)
            case _ => throw UnexpectedAsyncReference(rhs.pos, "Noncombinational method called in assign.")
          }
          case t@_ => throw UnexpectedType(mod.pos, "External Call", "Object Type", t)
        }
      case (EVar(_), ECall(_, None, _)) => //this is calling another PDL pipeline, not allowed in assign
        throw UnexpectedAsyncReference(rhs.pos, "no calls in assign")
      case (EVar(id), _) =>
        val (t1, nt1) = if (isRecv) { (vars, nextVars + id) } else { (vars + id, nextVars) }
        val memReads = getMemReads(rhs)
        memReads.foldLeft((t1, nt1))((t, m) => {
          if (isLockedMemory(m.mem)) { assignReadLocks(id, m, t._1, t._2) } else t
        })
      case (_ :EMemAccess, _ :EMemAccess) =>
        throw UnexpectedAsyncReference(lhs.pos, "Both sides of <- cannot be memory or modules references")
      //don't need to check handles for unlocked mems nor atomic, nor general mems
      case (e@EMemAccess(mem, _, _, inHandle, outHandle, false), _)
        if isLockedMemory(mem) && inHandle.isDefined && outHandle.isDefined =>
        checkExpr(inHandle.get, vars)
        val accessOp = if (isSynchronousAccess(e.mem, isWrite = true)) None else Some(LockWrite)
        LockImplementation.getAccess(LockImplementation.getLockImpl(e), accessOp) match
        {
          case Some((_, Combinational)) => (vars + outHandle.get.id, nextVars)
          case Some(_) => (vars, nextVars + outHandle.get.id)
          case None => throw UnsupportedLockOperation(mem, "Write", e.pos)
        }
      case _ => (vars, nextVars)
    }
  }
  /**
   * Check that the given command only uses variables which are currently available.
   * Produce a new environment that indicates the variables made available by this command,
   * both those available immediately and those available after a logical timestep.
   * @param c The command to check
   * @param vars The current set of available variables
   * @param nextVars The set of variables which will be available next timestep.
   * @return The updated sets of available variables
   */
  def checkCommand(c: Command, vars: Available, nextVars: Available): (Available, Available) = {

    c match {
      case CSeq(c1, c2) =>
        val (v2, nv2) = checkCommand(c1, vars, nextVars)
        checkCommand(c2, v2, nv2)
      case CTBar(c1, c2) =>
        val (v2, nv2) = checkCommand(c1, vars, nextVars)
        checkCommand(c2, v2 ++ nv2, NoneAvailable)
      case CSplit(cases, default) =>
        var (endv, endnv) = checkCommand(default, vars, nextVars)
        endnv = endnv.union(endv)
        var (cur_lock, next_lock) = (endv.filter(is_handle_var),
          endnv.filter(is_handle_var))
        for (c <- cases) {
          if(checkExpr(c.cond, vars) != Latency.Combinational) {
            throw UnexpectedAsyncReference(c.cond.pos, c.cond.toString)
          }
          val (v2, nv2) = checkCommand(c.body, vars, nextVars)
          cur_lock = cur_lock.union(v2.filter(is_handle_var))
          next_lock = next_lock.union(nv2.filter(is_handle_var))
          endv = endv.intersect(v2)
          endnv = endnv.intersect(nv2.union(v2))
        }
        cur_lock = cur_lock.diff(next_lock)
        (endv.union(cur_lock), endnv.union(next_lock))
      case CIf(cond, cons, alt) =>
        if(checkExpr(cond, vars) != Latency.Combinational) {
          throw UnexpectedAsyncReference(cond.pos, cond.toString)
        }
        val (vt, nvt) = checkCommand(cons, vars, nextVars)
        val (vf, nvf) = checkCommand(alt, vars, nextVars)
        val this_cycle_intersect = vt.intersect(vf)
        var cur_lock_handles = vt.filter(is_handle_var).union(
          vf.filter(is_handle_var))
        val next_lock_handles = nvt.filter(is_handle_var).union(
          nvf.filter(is_handle_var))
        cur_lock_handles = cur_lock_handles.diff(next_lock_handles)
        (this_cycle_intersect.union(cur_lock_handles),
          nvt.union(vt).intersect(nvf.union(vf)).removedAll(this_cycle_intersect).union(next_lock_handles))
      case CAssign(lhs, rhs) =>
        if (checkExpr(rhs, vars) != Latency.Combinational) {
          throw UnexpectedAsyncReference(rhs.pos, rhs.toString)
        }
        check_recv_or_asn(lhs, rhs, isRecv = false, vars, nextVars)
      case CRecv(lhs, rhs) => check_recv_or_asn(lhs, rhs, isRecv = true, vars, nextVars)
      case CLockStart(_) => (vars, nextVars)
      case CLockEnd(_) => (vars, nextVars)
      case CLockOp(mem, _, _, _, _) =>
        if (mem.evar.isDefined) {
          checkExpr(mem.evar.get, vars, isRhs = true)
        }
        (vars, nextVars)
      case CSpecCall(handle, _, args) =>
        //args must be available, but handle is available next cycle
        args.foreach(a => if(checkExpr(a, vars) != Combinational) {
          throw UnexpectedAsyncReference(a.pos, a.toString)
        })
        (vars, nextVars + handle.id)
      case CVerify(handle, args, preds, upd) =>
        //handle and args must be available this cycle
        if(checkExpr(handle, vars) != Combinational) {
          throw UnexpectedAsyncReference(handle.pos, handle.toString)
        }
        args.foreach(a => if(checkExpr(a, vars) != Combinational) {
          throw UnexpectedAsyncReference(a.pos, a.toString)
        })
        //just don't check preds they get inserted by the compiler automatically
        if (upd.isDefined) {
          if (checkExpr(upd.get, vars) != Combinational) {
            throw UnexpectedAsyncReference(handle.pos, handle.toString)
          }
        }
        (vars, nextVars)
      case CUpdate(nh, handle, args, preds) =>
        if(checkExpr(handle, vars) != Combinational) {
          throw UnexpectedAsyncReference(handle.pos, handle.toString)
        }
        args.foreach(a => if(checkExpr(a, vars) != Combinational) {
          throw UnexpectedAsyncReference(a.pos, a.toString)
        })
        //just don't check preds they get inserted by the compiler automatically
        (vars, nextVars + nh.id)
      case CInvalidate(handle) =>
        if(checkExpr(handle, vars) != Combinational) {
          throw UnexpectedAsyncReference(handle.pos, handle.toString)
        }
        (vars, nextVars)
      case CCheckSpec(_) => (vars, nextVars)
      case COutput(exp) =>
        if (checkExpr(exp, vars) != Combinational) {
          throw UnexpectedAsyncReference(exp.pos, exp.toString)
        }
        (vars, nextVars)
      case CReturn(exp) =>
        if (checkExpr(exp, vars) != Combinational) {
          throw UnexpectedAsyncReference(exp.pos, exp.toString)
        }
        (vars, nextVars)
      case CExpr(exp) =>
        checkExpr(exp, vars)
        (vars, nextVars)
      case Syntax.CEmpty() => (vars, nextVars)
      case CPrint(args) =>
        args.foreach(a => {
          checkExpr(a, vars)
        })
        (vars, nextVars)
      case i@IReserveLock(outHandle, mem) =>
        LockImplementation.getReserve(LockImplementation.getLockImpl(mem), i.memOpType) match {
          case Some((_, Combinational)) => (vars + outHandle.id, nextVars)
          case Some(_) => (vars, nextVars + outHandle.id)
          case None => throw UnsupportedLockOperation(mem.id, "Reserve" + i.memOpType.getOrElse(""), i.pos)
        }
      case i@ICheckLockOwned(mem, inHandle, outHandle) =>
        checkExpr(inHandle, vars)
        LockImplementation.getBlock(LockImplementation.getLockImpl(mem), i.memOpType) match {
          case Some((_, Combinational)) => (vars + outHandle.id, nextVars)
          case Some(_) => (vars, nextVars + outHandle.id)
          case None => (vars + outHandle.id, nextVars)
          //assume combinational for this since it means you don't need to block
        }
      case IReleaseLock(_, inHandle) => //timing of release doesn't really matter, result can't be used
        checkExpr(inHandle, vars)
        (vars, nextVars)
      case _ => throw UnexpectedCommand(c)
    }
  }

  def checkExpr(e: Expr, vars: Available, isRhs: Boolean = true): Latency = e match {
    case EUop(_, e) => checkExpr(e, vars, isRhs) match {
      case Combinational => Combinational
      case _ => throw UnexpectedAsyncReference(e.pos, e.toString)
    }
    case EBinop(_, e1, e2) =>
      (checkExpr(e1, vars, isRhs), checkExpr(e2, vars, isRhs)) match {
        case (Combinational, Combinational) => Combinational
        case (Combinational, _) => throw UnexpectedAsyncReference(e2.pos, e2.toString)
        case (_, Combinational) => throw UnexpectedAsyncReference(e1.pos, e1.toString)
        case (_,_) => throw UnexpectedAsyncReference(e1.pos, e1.toString)
      }
    case ERecAccess(rec, _) => checkExpr(rec, vars, isRhs) match {
      case Combinational => Combinational
      case _ => throw UnexpectedAsyncReference(rec.pos, rec.toString)
    }
    case EMemAccess(m, index, wm, inHandle, outHandle, isAtomic) =>
      def checkMemRead(rLat: Latency, wLat: Latency): Latency = {
        val memLat = if (isRhs) { rLat } else { wLat }
        val indexExpr = checkExpr(index, vars, isRhs)
        if (wm.isDefined) {
          checkExpr(wm.get, vars, isRhs) match {
            case Combinational => ()
            case _ => throw UnexpectedAsyncReference(wm.get.pos, wm.get.toString)
          }
        }
        indexExpr match {
          case Combinational => memLat
          case _ => throw UnexpectedAsyncReference(index.pos, index.toString)
        }
      }
      m.typ.get match {
      case TMemType(_, _, rLat, wLat, _, _) =>
        checkMemRead(rLat, wLat)
      case TLockedMemType(TMemType(_, _, rLat, wLat, _, _),_,_) =>
        checkMemRead(rLat, wLat)
      case _ => throw UnexpectedType(m.pos, m.v, "Mem Type", m.typ.get)
    }
    case EBitExtract(num, _, _) => checkExpr(num, vars, isRhs) match {
      case Combinational => Combinational
      case _ => throw UnexpectedAsyncReference(num.pos, num.toString)
    }
    case ETernary(cond, tval, fval) =>
      (checkExpr(cond, vars, isRhs),
        checkExpr(tval, vars, isRhs),
        checkExpr(fval, vars, isRhs)) match {
        case (Combinational, Combinational, Combinational) =>
          Combinational
        case (_, Combinational, Combinational) => throw UnexpectedAsyncReference(cond.pos, cond.toString)
        case (_, _, Combinational) => throw UnexpectedAsyncReference(tval.pos, tval.toString)
        case _ => throw UnexpectedAsyncReference(fval.pos, fval.toString)
      }
    case EApp(_, args) =>
      args.foreach(a => if(checkExpr(a, vars) != Combinational) {
        throw UnexpectedAsyncReference(a.pos, a.toString)
      })
      Combinational
    case ECall(mod, name, args) =>
      args.foreach(a => if(checkExpr(a, vars) != Combinational) {
        throw UnexpectedAsyncReference(a.pos, a.toString)
      })
      //TODO methods are hacked
      name match
      {
        case Some(nm) => mod.typ match {
          case Some(TObject(_, _, methods)) => methods(nm)._2
          case Some(a) => throw UnexpectedType(e.pos, "Method call", "Object type", a)
          case None => throw MissingType(e.pos, "Method call")
        }
        case None => Asynchronous
      }
    case EVar(id) => if(!vars(id) && isRhs) {
      throw UnavailableArgUse(e.pos, id.toString) }
      //TODO make this error message more clear about what's wrong when these are lock handle vars
      else { Combinational }
    case ECast(_, exp) => checkExpr(exp, vars, isRhs)
    case _ => Combinational
  }
  //No timing in the circuit, just connections
  override def checkCircuit(c: Circuit, env: Environment[Id, Type]): Environment[Id, Type] = env

}
