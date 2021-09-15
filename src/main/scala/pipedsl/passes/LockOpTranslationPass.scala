package pipedsl.passes

import pipedsl.common.DAGSyntax.PStage
import pipedsl.common.Errors.UnexpectedCommand
import pipedsl.common.Locks.{Free, Reserved, _}
import pipedsl.common.{LockImplementation, Locks}
import pipedsl.common.Syntax._
import pipedsl.common.Utilities.{flattenStageList, mkOr, updateListMap}
import pipedsl.passes.Passes.{CommandPass, ModulePass, ProgPass, StagePass}

import scala.+:


/**
 * This pass converts the high level (syntax-level) lock operations
 * into the operational realization of lock operations. Specifically,
 * this involves generating variables to represent the lock state information
 * held by the Thread. Reserve statements will produce these 'handles' which
 * must be passed to future operations like 'block' and 'release'
 *
 * This additionally annotates memory accesses with the appropriate lock handle variable,
 * s.t. implementations that feed the memory accesses through the lock API have the information
 * they need to generate code.
 */
object LockOpTranslationPass extends ProgPass[Prog] with CommandPass[Command] with ModulePass[ModuleDef]{


  private def lockVar(l: LockArg, op :LockedMemState.LockedMemState): EVar = {
    val lockname = "_lock_id_" + l.id.v + (if (l.evar.isDefined) "_" + l.evar.get.id.v else "") + (op match
    {
      case LockedMemState.Free     => "_fr"
      case LockedMemState.Reserved => "_rs"
      case LockedMemState.Acquired => "_aq"
      case LockedMemState.Operated => "_op"
      case LockedMemState.Released => "_rl"
    })
    //val other = op --
    val res = EVar(Id(lockname))
    res.typ = Some(TMaybe(TRequestHandle(l.id, RequestType.Lock)))
    res.id.typ = res.typ
    res
  }

  object LockedMemState extends Enumeration
  {
    type LockedMemState = Value
    val Free, Reserved, Acquired, Operated, Released = Value
    def -- :LockedMemState = this match {
      case Reserved => Free
      case Acquired => Reserved
      case Operated => Acquired
      case Released => Operated
    }
  }
  private def lk_st_2_mem_st(st :LockState) = st match
  {
    case Locks.Free => LockedMemState.Free
    case Locks.Reserved => LockedMemState.Reserved
    case Locks.Acquired => LockedMemState.Acquired
    case Locks.Released => LockedMemState.Released
  }

  /*
  override def run(stgs: List[PStage]): List[PStage] = {
    flattenStageList(stgs).foreach(s => {
      eliminateLockRegions(s)
      translateLockOps(s)
    })
    stgs
  }
*/ override def run(p: Prog): Prog =
    {
      p.copy(moddefs = p.moddefs.map(run))
    }

  override def run(m: ModuleDef): ModuleDef =
    {
      val nm = m.copy(body = run(m.body))
      nm.isRecursive = m.isRecursive
      nm.maybeSpec = m.maybeSpec
      nm
    }

  override def run(c: Command): Command = c match
  {
    case CSeq(c1, c2) => CSeq(run(c1), run(c2)).copyMeta(c)
    case CTBar(c1, c2) => CTBar(run(c1), run(c2)).copyMeta(c)
    case CIf(cond, cons, alt) => CIf(modifyMemArg(cond, isLhs = false), run(cons), run(alt)).copyMeta(c)
    case CAssign(lhs, rhs) => CAssign(lhs, modifyMemArg(rhs, isLhs = false)).copyMeta(c)
    case CRecv(lhs, rhs) => CRecv(modifyMemArg(lhs, isLhs = true), modifyMemArg(rhs, isLhs = false)).copyMeta(c)
    case CSpecCall(handle, pipe, args) => CSpecCall(handle, pipe, args.map(modifyMemArg(_, isLhs = false))).copyMeta(c)
    case CVerify(handle, args, preds, update) => CVerify(handle, args.map(modifyMemArg(_, isLhs = false)), preds, update.map(modifyMemArg(_, isLhs = false).asInstanceOf[ECall])).copyMeta(c)
    case CUpdate(newHandle, handle, args, preds) => CUpdate(newHandle, handle, args.map(modifyMemArg(_, isLhs = false)), preds).copyMeta(c)
    case CPrint(args) => CPrint(args.map(modifyMemArg(_, isLhs = false))).copyMeta(c)
    case COutput(exp) =>  COutput(modifyMemArg(exp, isLhs = false)).copyMeta(c)
    case CReturn(exp) => CReturn(modifyMemArg(exp, isLhs = false)).copyMeta(c)
    case CExpr(exp) => CExpr(modifyMemArg(exp, isLhs = false)).copyMeta(c)
    case c :CLockOp => translateOp(c)
    case CSplit(cases, default) => CSplit(cases.map(cobj => CaseObj(cond = modifyMemArg(cobj.cond, isLhs = false), body = run(cobj.body))), run(default)).copyMeta(c)
    case other => other
  }


  /**
   * Replace the existing lock ops where necessary.
   * Specifically, add in "lock check" commands that block
   * until the lock is acquirable and remove the "acquire"
   * commands that are unnecessary thanks to the lock already being "reserved"
   *
   * @param stg - The stage to modify
   */
  /*private def translateLockOps(stg: PStage): Unit = {
    val (lockCmds, notlockCmds) = stg.getCmds.partition { case _: CLockOp => true; case _ => false }
    val lockmap = lockCmds.foldLeft(Map[LockArg, List[Command]]())((m, lc) => lc match {
      case c@CLockOp(mem, _, _, _, _) => updateListMap(m, mem, translateOp(c))
      case _ => throw UnexpectedCommand(lc)
    })
    val newlockcmds = lockmap.keys.foldLeft(List[Command]())((cs, lid) => {
      val lockImpl = LockImplementation.getLockImpl(lid)
      cs ++ lockmap(lid)//lockImpl.mergeLockOps(lid, lockmap(lid))
    })
    val modNotLockCmds = notlockCmds.foldLeft[Command](CEmpty())((lc, c) => CSeq(lc, modifyMemOpsArgs(c)))
    stg.setCmds(modNotLockCmds ++ newlockcmds)
  }*/

  private def modifyMemArg(e: Expr, isLhs: Boolean): Expr = e match {
    //no translation needed here since locks aren't address specific
    //case em@EMemAccess(_, _, _, _, _) if em.granularity == General => println("dumbass"); em
    //SimplifyRecvPass ensures that the index expression is always a variable
    case em@EMemAccess(mem, idx@EVar(_), wm, inHandle, outHandle) /*if em.granularity == Specific*/ =>
      val l_arg = LockArg(mem, em.granularity match
      { case Locks.Specific => Some(idx)
        case Locks.General => None
      })
      val newArg: Expr = idx//modifyMemAddrArg(isLhs, mem, idx)
      val res = EMemAccess(mem, newArg, wm, Some(lockVar(l_arg, LockedMemState.Acquired)), Some(lockVar(l_arg, LockedMemState.Operated))).setPos(em.pos)
      res.copyMeta(em)
    case et@ETernary(cond, tval, fval) =>
      val ncond = modifyMemArg(cond, isLhs)
      val ntval = modifyMemArg(tval, isLhs)
      val nfval = modifyMemArg(fval, isLhs)
      val ntern = ETernary(ncond, ntval, nfval).setPos(et.pos)
      ntern.typ = et.typ
      ntern
    case ee@EUop(op, ex) =>
      val nop = EUop(op, modifyMemArg(ex, isLhs)).setPos(ee.pos)
      nop.typ = ee.typ
      nop
    case ea@EApp(f, args) =>
      val newargs = args.foldLeft(List[Expr]())((args, a) => args :+ modifyMemArg(a, isLhs))
      val newapp = EApp(f, newargs).setPos(ea.pos)
      newapp.typ = ea.typ
      newapp
    case ec@ECall(p, name, args) =>
      val newargs = args.foldLeft(List[Expr]())((args, a) => args :+ modifyMemArg(a, isLhs))
      val newcall = ECall(p, name, newargs).setPos(ec.pos)
      newcall.typ = ec.typ
      newcall
    case eb@EBinop(o, e1, e2) =>
      val ne1 = modifyMemArg(e1, isLhs)
      val ne2 = modifyMemArg(e2, isLhs)
      val newbin = EBinop(o, ne1, ne2).setPos(eb.pos)
      newbin.typ = eb.typ
      newbin
    case ec@ECast(t, exp) =>
      val ncast = ECast(t, modifyMemArg(exp, isLhs)).setPos(ec.pos)
      ncast.typ = ec.typ
      ncast
    case _ => e
  }

  private def modifyMemAddrArg(isWrite: Boolean, mem: Id, idx: EVar): Expr = {
    val larg = LockArg(mem, Some(idx))
    val lock = EFromMaybe(lockVar(larg, LockedMemState.Operated)).setPos(larg.pos)
    lock.typ = lock.ex.typ.get.matchOrError(lock.ex.pos, "Extract Lock Handle", "Maybe(Handle)") {
      case TMaybe(t) => Some(t)
    }
    val newArg = if (isWrite) LockImplementation.getLockImpl(larg).getWriteArgs(idx, lock)
    else LockImplementation.getLockImpl(larg).getReadArgs(idx, lock)
    newArg
  }

  private def modifyMemOpsArgs(c: Command): Command = {
    c match {
      //SimplifyRecvPass ensures that rhs is always a MemAccess xor a complex expression w/o memaccesses
      case CAssign(lhs, rhs) =>
        val nrhs = modifyMemArg(rhs, isLhs = false)
        CAssign(lhs, nrhs).setPos(c.pos)
      case CRecv(lhs, rhs) =>
        val nlhs = modifyMemArg(lhs, isLhs = true)
        val nrhs = modifyMemArg(rhs, isLhs = false)
        CRecv(nlhs, nrhs).setPos(c.pos)
      //lhs should not contain _any_ memaccesses thanks to the ConvertAsyncPass (but adding this doesn't hurt)
      case CExpr(exp) => CExpr(modifyMemArg(exp, isLhs = false)).setPos(c.pos)
    /*  case im :IMemSend if im.granularity == General => im
      case im@IMemSend(h, writeMask, mem, d, addr) if im.granularity == Specific =>
        val newAddr = modifyMemAddrArg(im.isWrite, mem, addr)
        //TODO find cleaner WAY than this
        val newVar = EVar(
          Id("_tmp_" + mem.v + "_" + addr.id.v + "_lock_var_" + (if (im.isWrite) "W" else "R"))
        ).setPos(im.pos)
        newVar.typ = newAddr.typ
        newVar.id.typ = newVar.typ
        val newSend = IMemSend(h, writeMask, mem, d, newVar, , ).setPos(im.pos)
        newSend.portNum = im.portNum
        val newAssn = CAssign(newVar, newAddr).setPos(im.pos)
        CSeq(newAssn, newSend)
      case im@IMemWrite(_, _, _) if im.granularity == General => im
      case im@IMemWrite(mem, addr, data) if im.granularity == Specific =>
        val newAddr = modifyMemAddrArg(isWrite = true, mem, addr)
        //TODO find cleaner WAY than this
        val newVar = EVar(Id("_tmp_" + mem.v + "_" + addr.id.v + "_lock_var_W")).setPos(im.pos)
        newVar.typ = newAddr.typ
        newVar.id.typ = newVar.typ
        val newSend = IMemWrite(mem, newVar, data, , ).setPos(im.pos)
        newSend.portNum = im.portNum
        val newAssn = CAssign(newVar, newAddr).setPos(im.pos)
        CSeq(newAssn, newSend)
    */
      case _ => c
    }
  }

  private def translateOp(c: CLockOp): Command = {
  /*  c.op match
    {
      case Locks.Free =>
        c
      case Locks.Reserved =>
        c.copy(ret = Some(lockVar(c.mem, LockedMemState.Reserved))).copyMeta(c)
      case Locks.Acquired =>
        c.copy(ret = Some(lockVar(c.mem, LockedMemState.Acquired)), args = lockVar(c.mem, LockedMemState.Reserved) :: c.args )
      case Locks.Released =>
        c.copy(args = lockVar(c.mem, LockedMemState.Operated) :: c.args)
    }*/

  //TODO make this cleaner lol
    c.op match {
      case Locks.Free =>
        val i = ICheckLockFree(c.mem).setPos(c.pos)
        i.memOpType = c.memOpType
        i.granularity = c.granularity
        i.portNum = c.portNum
        i
      case Locks.Reserved =>
        val i = IReserveLock(lockVar(c.mem, LockedMemState.Reserved), c.mem).setPos(c.pos)
        i.memOpType = c.memOpType
        i.granularity = c.granularity
        i.portNum = c.portNum
        i
      case Locks.Acquired =>
        val i = ICheckLockOwned(c.mem,
          inHandle = lockVar(c.mem, LockedMemState.Reserved),
          outHandle = lockVar(c.mem, LockedMemState.Acquired)).setPos(c.pos)
        i.memOpType = c.memOpType
        i.granularity = c.granularity
        i.portNum = c.portNum
        i
      case Locks.Released =>
        val i = IReleaseLock(c.mem, inHandle = lockVar(c.mem, LockedMemState.Operated)).setPos(c.pos)
        i.memOpType = c.memOpType
        i.granularity = c.granularity
        i.portNum = c.portNum
        i
    }

  }

}
