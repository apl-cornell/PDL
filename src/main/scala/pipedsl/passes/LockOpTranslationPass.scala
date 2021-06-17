package pipedsl.passes

import pipedsl.common.DAGSyntax.PStage
import pipedsl.common.Errors.UnexpectedCommand
import pipedsl.common.Locks._
import pipedsl.common.LockImplementation
import pipedsl.common.Syntax._
import pipedsl.common.Utilities.{flattenStageList, updateListMap}
import pipedsl.passes.Passes.StagePass


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
object LockOpTranslationPass extends StagePass[List[PStage]] {


  private def lockVar(l: LockArg): EVar = {
    val lockname = "_lock_id_" + l.id.v + (if (l.evar.isDefined) "_" + l.evar.get.id.v else "")
    val res = EVar(Id(lockname))
    res.typ = Some(TMaybe(TRequestHandle(l.id, RequestType.Lock)))
    res.id.typ = res.typ
    res
  }

  override def run(stgs: List[PStage]): List[PStage] = {
    flattenStageList(stgs).foreach(s => {
      eliminateLockRegions(s)
      translateLockOps(s)
    })
    stgs
  }

  /**
   * Replace the existing lock ops where necessary.
   * Specifically, add in "lock check" commands that block
   * until the lock is acquirable and remove the "acquire"
   * commands that are unnecessary thanks to the lock already being "reserved"
   *
   * @param stg - The stage to modify
   */
  private def translateLockOps(stg: PStage): Unit = {
    val (lockCmds, notlockCmds) = stg.getCmds.partition { case _: CLockOp => true; case _ => false }
    val lockmap = lockCmds.foldLeft(Map[LockArg, List[Command]]())((m, lc) => lc match {
      case c@CLockOp(mem, _, _) => updateListMap(m, mem, translateOp(c))
      case _ => throw UnexpectedCommand(lc)
    })
    val newlockcmds = lockmap.keys.foldLeft(List[Command]())((cs, lid) => {
      val lockImpl = LockImplementation.getLockImpl(lid)
      cs ++ lockImpl.mergeLockOps(lid, lockmap(lid))
    })
    val modNotLockCmds = notlockCmds.foldLeft(List[Command]())((lc, c) => lc ++ modifyMemOpsArgs(c))
    stg.setCmds(modNotLockCmds ++ newlockcmds)
  }

  private def modifyMemArg(e: Expr, isLhs: Boolean): Expr = e match {
    //no translation needed here since locks aren't address specific
    case em@EMemAccess(_, _, _) if em.granularity == General => em
    //SimplifyRecvPass ensures that the index expression is always a variable
    case em@EMemAccess(mem, idx@EVar(_), wm) if em.granularity == Specific =>
      val newArg: Expr = modifyMemAddrArg(isLhs, mem, idx)
      val res = EMemAccess(mem, newArg, wm).setPos(em.pos)
      res.typ = em.typ
      res.memOpType = em.memOpType
      res
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
    case ec@ECall(p, args) =>
      val newargs = args.foldLeft(List[Expr]())((args, a) => args :+ modifyMemArg(a, isLhs))
      val newcall = ECall(p, newargs).setPos(ec.pos)
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
    val lock = EFromMaybe(lockVar(larg)).setPos(larg.pos)
    lock.typ = lock.ex.typ.get.matchOrError(lock.ex.pos, "Extract Lock Handle", "Maybe(Handle)") {
      case TMaybe(t) => Some(t)
    }
    val newArg = if (isWrite) LockImplementation.getLockImpl(larg).getWriteArgs(idx, lock)
    else LockImplementation.getLockImpl(larg).getReadArgs(idx, lock)
    newArg
  }

  private def modifyMemOpsArgs(c: Command): List[Command] = {
    c match {
      //SimplifyRecvPass ensures that rhs is always a MemAccess xor a complex expression w/o memaccesses
      case CAssign(lhs, rhs) =>
        val nrhs = modifyMemArg(rhs, isLhs = false)
        List(CAssign(lhs, nrhs).setPos(c.pos))
      case CRecv(lhs, rhs) =>
        val nlhs = modifyMemArg(lhs, isLhs = true)
        val nrhs = modifyMemArg(rhs, isLhs = false)
        List(CRecv(nlhs, nrhs).setPos(c.pos))
      //lhs should not contain _any_ memaccesses thanks to the ConvertAsyncPass (but adding this doesn't hurt)
      case CExpr(exp) => List(CExpr(modifyMemArg(exp, isLhs = false)).setPos(c.pos))
      case im@IMemSend(_, _, _, _, _) if im.granularity == General => List(im)
      case im@IMemSend(h, writeMask, mem, d, addr) if im.granularity == Specific =>
        val newAddr = modifyMemAddrArg(im.isWrite, mem, addr)
        //TODO find cleaner WAY than this
        val newVar = EVar(
          Id("_tmp_" + mem.v + "_" + addr.id.v + "_lock_var_" + (if (im.isWrite) "W" else "R"))
        ).setPos(im.pos)
        newVar.typ = newAddr.typ
        newVar.id.typ = newVar.typ
        val newSend = IMemSend(h, writeMask, mem, d, newVar).setPos(im.pos)
        val newAssn = CAssign(newVar, newAddr).setPos(im.pos)
        List(newAssn, newSend)
      case im@IMemWrite(_, _, _) if im.granularity == General => List(im)
      case im@IMemWrite(mem, addr, data) if im.granularity == Specific =>
        val newAddr = modifyMemAddrArg(isWrite = true, mem, addr)
        //TODO find cleaner WAY than this
        val newVar = EVar(Id("_tmp_" + mem.v + "_" + addr.id.v + "_lock_var_W")).setPos(im.pos)
        newVar.typ = newAddr.typ
        newVar.id.typ = newVar.typ
        val newSend = IMemWrite(mem, newVar, data).setPos(im.pos)
        val newAssn = CAssign(newVar, newAddr).setPos(im.pos)
        List(newAssn, newSend)
      case _ => List(c)
    }
  }

  private def translateOp(c: CLockOp): Command = {

  //TODO make this cleaner lol
    c.op match {
      case Free =>
        val i = ICheckLockFree(c.mem).setPos(c.pos)
        i.memOpType = c.memOpType
        i.granularity = c.granularity
        i
      case Reserved =>
        val i = IReserveLock(lockVar(c.mem), c.mem).setPos(c.pos)
        i.memOpType = c.memOpType
        i.granularity = c.granularity
        i
      case Acquired =>
        val i = ICheckLockOwned(c.mem, lockVar(c.mem)).setPos(c.pos)
        i.memOpType = c.memOpType
        i.granularity = c.granularity
        i
      case Released =>
        val i = IReleaseLock(c.mem, lockVar(c.mem)).setPos(c.pos)
        i.memOpType = c.memOpType
        i.granularity = c.granularity
        i
    }

  }

}
