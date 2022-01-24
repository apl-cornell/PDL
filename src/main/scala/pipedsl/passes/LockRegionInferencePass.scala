package pipedsl.passes

import pipedsl.common.Locks.Reserved
import pipedsl.common.Syntax._
import pipedsl.common.Utilities.{andExpr, orExpr}
import pipedsl.passes.Passes.{ModulePass, ProgPass}

/**
 * This class automatically infers where to place lock region start ([[pipedsl.common.Syntax.CLockStart(mod)]],
 * and end ([[pipedsl.common.Syntax.CLockEnd(mod)]]) statements.
 * These are inferred based on the following algorithm:
 *  - Place one [[pipedsl.common.Syntax.CLockStart]] statement for each memory, _as late as possible_ such that
 *  it still dominates all atomic operations and lock reservation statements for that memory.
 *  - Place one [[pipedsl.common.Syntax.CLockEnd]] statement for each memory, _as early as possible_ such tath
 *  it is still dominated by all atomic operations and lock reservation statements for that memory.
 *
 *  Additionally, place a [[pipedsl.common.Syntax.CCheckpoint]] statement right before the
 *  [[pipedsl.common.Syntax.CLockEnd]] statement, conditionally executed when checkpoints
 *  are necessary. Specifically, checkpoints are necessary on any program path when
 *  the same program path will lead to a [[pipedsl.common.Syntax.CInvalidate]],
 *  [[pipedsl.common.Syntax.CVerify]], or [[pipedsl.common.Syntax.CUpdate]] command.
 *
 *  We find these program paths by using Z3 to generate the set of conditions
 *  under which these are true and generate a condition by OR-ing them together.
 *
 * @param ctx
 */
class LockRegionInferencePass() extends ModulePass[ModuleDef] with ProgPass[Prog] {

  override def run(p: Prog): Prog = p.copy(exts = p.exts, fdefs = p.fdefs,
    moddefs = p.moddefs.map(m => run(m))).setPos(p.pos)

  private var unlockedMems = Set[Id]()
  private var checkpointConds = Map[Id, Option[Expr]]()

  override def run(m: ModuleDef): ModuleDef = {
    unlockedMems = Set()
    checkpointConds = Map()
    m.modules.foreach(p => {
      p.typ match {
        case TMemType(_, _, _, _, _, _) =>
          unlockedMems = unlockedMems + p.name
        case _ => ()
      }
    })
    val modIds = m.modules.map(p => p.name).toSet
    val starts = insertStarts(m.body, modIds, Set())._1
    val ends = insertEnds(starts, modIds, Set())._1
    findInvalidateConds(m.body, None, (Set(), Set())) //update the checkpointConds map
    //don't try inference for any mods that already have a checkpoint or don't need one
    checkpointConds = checkpointConds.removedAll(haveCheckpoint(m.body))
    val (memsThatNeedChks, _) = needsCheckpoint(m.body, m.maybeSpec)
    val checks = insertChecks(ends, modIds.intersect(checkpointConds.keySet).intersect(memsThatNeedChks))
    m.copy(body = checks._1).copyMeta(m)
  }

  /**
   * Add in the [[pipedsl.common.Syntax.CLockStart]] commands for the modules
   * specified in `mods` as late as possible such that it still occurs before
   * any atomic memory accesses for the given mem, or reservations for the given mem.
   * @param c The command to modify
   * @param mods The set of lock regions we plan to add by module name
   * @param resedLater The set of ids which still get reserved later in the program
   * @return (Modified command, set of START regions added)
   */
  private def insertStarts(c: Command, mods: Set[Id], resedLater: Set[Id]): (Command, Set[Id]) = c match {
    case CSeq(c1, c2) =>
      val (c1p, addedLeft) = insertStarts(c1, mods, resedLater ++ getResRegionIds(c2))
      val (c2p, addedRight) = insertStarts(c2, mods -- addedLeft, resedLater)
      val cp = CSeq(c1p, c2p).setPos(c.pos)
      (cp, addedLeft ++ addedRight)
    case CTBar(c1, c2) =>
      val (c1p, addedLeft) = insertStarts(c1, mods, resedLater ++ getResRegionIds(c2))
      val (c2p, addedRight) = insertStarts(c2, mods -- addedLeft, resedLater)
      val cp = CTBar(c1p, c2p).setPos(c.pos)
      (cp, addedLeft ++ addedRight)
    case CIf(cond, cons, alt) =>
      val addedInCond = getAtomicAccessIds(cond).intersect(mods)
      val consIds = getResRegionIds(cons).intersect(mods)
      val altIds = getResRegionIds(alt).intersect(mods)
      //if ID reserved/atomic accessed in cond, OR in both cons & alt (or in a branch
      // and in the code following the if, then add before
      val addBefore = (addedInCond ++
        consIds.intersect(altIds) ++ consIds.intersect(resedLater) ++ altIds.intersect(resedLater)).intersect(mods)
      val (consp, addedCons) = insertStarts(cons, mods -- addBefore, resedLater)
      val (altp, addedAlt) = insertStarts(alt, mods -- addBefore, resedLater)
      val cp = CIf(cond, consp, altp).setPos(c.pos)
      val cpAdded = addLockStarts(addBefore, cp)
      (cpAdded, addBefore ++ addedCons ++ addedAlt)
    case CSplit(cases, default) =>
      val addedInConds = cases.foldLeft(Set[Id]())((s, cs) => {
        s ++ getAtomicAccessIds(cs.cond)
      }).intersect(mods)
      val addedInBranch = (cases.map(cs => cs.body) :+ default).map(cmd => getResRegionIds(cmd).intersect(mods)) :+ resedLater
      //if ID reserved/atomic accessed in any cond OR in any two branches (including after the program), then add before
      var addBefore = addedInConds
      //for each branch, check if it intersects with union of all others (i know it's n^2...sad)
      for (i <- addedInBranch.indices) {
        val rest = (addedInBranch.take(i) ++ addedInBranch.drop(i + 1)).reduce((s1, s2) => s1 ++ s2)
        addBefore = addBefore ++ addedInBranch(i).intersect(rest)
      }
      addBefore = addBefore.intersect(mods)
      var allAdded = addBefore
      val ncases = cases.map(cs => {
        val (nbody, added) = insertStarts(cs.body, mods -- addBefore, resedLater)
        allAdded = allAdded ++ added
        CaseObj(cs.cond, nbody).setPos(cs.pos)
      })
      val (ndefault, defAdded) = insertStarts(default, mods -- addBefore, resedLater)
      allAdded = allAdded ++ defAdded
      val cp = CSplit(ncases, ndefault).setPos(c.pos)
      val cpAdded = addLockStarts(addBefore, cp)
      (cpAdded, allAdded)
      //if there already is a start, just say we added it
    case CLockStart(mem) => (c, Set(mem))
    case _ =>
      val toAdd = getImmediateResRegionIds(c).intersect(mods)
      (addLockStarts(toAdd, c), toAdd)
  }

  /**
   * Add in the [[pipedsl.common.Syntax.CLockEnd]] commands for the modules
   * specified in `mods` as early as possible such that it still occurs after
   * any atomic memory accesses for the given mem, or reservations for the given mem.
   * @param c The command to modify
   * @param mods The set of lock regions we plan to add by module name
   * @param resedBefore The set of lock regions reserved before this statement, used
   *                    to push ends outside of branches when some reserves happen before the start of the if.
   * @return (Modified command, set of END regions added)
   */
  private def insertEnds(c: Command, mods: Set[Id], resedBefore: Set[Id]): (Command, Set[Id]) = c match {
    //this one recurses on the right first and adds things afterwards (opposite of insertStarts)
    case CSeq(c1, c2) =>
      val (c2p, addedRight) = insertEnds(c2, mods, resedBefore ++ getResRegionIds(c1))
      val (c1p, addedLeft) = insertEnds(c1, mods -- addedRight, resedBefore)
      val cp = CSeq(c1p, c2p).setPos(c.pos)
      (cp, addedLeft ++ addedRight)
    case CTBar(c1, c2) =>
      val (c2p, addedRight) = insertEnds(c2, mods, resedBefore ++ getResRegionIds(c1))
      val (c1p, addedLeft) = insertEnds(c1, mods -- addedRight, resedBefore)
      val cp = CTBar(c1p, c2p).setPos(c.pos)
      (cp, addedLeft ++ addedRight)
    case CIf(cond, cons, alt) =>
      val usedInCond = getAtomicAccessIds(cond).intersect(mods)
      val consIds = getResRegionIds(cons).intersect(mods)
      val altIds = getResRegionIds(alt).intersect(mods)
      val addedAfter = (usedInCond ++ consIds.intersect(altIds) ++
        consIds.intersect(resedBefore) ++ altIds.intersect(resedBefore)).intersect(mods)
      val (consp, addedCons) = insertEnds(cons, mods -- addedAfter, resedBefore)
      val (altp, addedAlt) = insertEnds(alt, mods -- addedAfter, resedBefore)
      val cp = CIf(cond, consp, altp).setPos(c.pos)
      val cpAdded = addLockEnds(addedAfter, cp)
      (cpAdded, addedAfter ++ addedCons ++ addedAlt)
    case CSplit(cases, default) =>
      val addedInConds = cases.foldLeft(Set[Id]())((s, cs) => {
        s ++ getAtomicAccessIds(cs.cond)
      }).intersect(mods)
      val addedInBranch = (cases.map(cs => cs.body) :+ default).map(cmd => getResRegionIds(cmd).intersect(mods)) :+ resedBefore
      //if ID reserved/atomic accessed in any cond OR in any two branches, then add after
      var addAfter = addedInConds
      //for each branch, check if it intersects with union of all others (i know it's n^2...sad)
      for (i <- addedInBranch.indices) {
        val rest = (addedInBranch.take(i) ++ addedInBranch.drop(i + 1)).reduce((s1, s2) => s1 ++ s2)
        addAfter = addAfter ++ addedInBranch(i).intersect(rest)
      }
      addAfter = addAfter.intersect(mods)
      var allAdded = addAfter
      val ncases = cases.map(cs => {
        val (nbody, added) = insertEnds(cs.body, mods -- addAfter, resedBefore)
        allAdded = allAdded ++ added
        CaseObj(cs.cond, nbody).setPos(cs.pos)
      })
      val (ndefault, defAdded) = insertEnds(default, mods -- addAfter, resedBefore)
      allAdded = allAdded ++ defAdded
      val cp = CSplit(ncases, ndefault).setPos(c.pos)
      val cpAdded = addLockEnds(addAfter, cp)
      (cpAdded, allAdded)
    //if there already is an end, just say we added it
    case CLockEnd(mem) => (c, Set(mem))
    case _ =>
      val toAdd = getImmediateResRegionIds(c).intersect(mods)
      (addLockEnds(toAdd, c), toAdd)
  }

  private def checkHandle(lid: Id): EVar = {
    val cid = Id("_checkpoint_" + lid.v)
    val handleVar = EVar(cid)
    handleVar.typ = Some(TRequestHandle(lid, RequestType.Checkpoint))
    cid.typ = handleVar.typ
    handleVar
  }

  //add checkpoint right before `end(rf)` if not already added
  private def insertChecks(c: Command, mods: Set[Id]): (Command, Set[Id]) = c match {
    case CSeq(c1, c2) =>
      val (c1p, addedleft) = insertChecks(c1, mods)
      val (c2p, addedRight) = insertChecks(c2, mods -- addedleft)
      (CSeq(c1p, c2p).setPos(c.pos), addedleft ++ addedRight)
    case CTBar(c1, c2) =>
      val (c1p, addedleft) = insertChecks(c1, mods)
      val (c2p, addedRight) = insertChecks(c2, mods -- addedleft)
      (CTBar(c1p, c2p).setPos(c.pos), addedleft ++ addedRight)
    case CIf(cond, cons, alt) =>
      val (c1p, addedleft) = insertChecks(cons, mods)
      val (c2p, addedRight) = insertChecks(alt, mods)
      (CIf(cond, c1p, c2p).setPos(c.pos), addedleft ++ addedRight)
    case CSplit(cases, default) =>
      var ncases = List[CaseObj]()
      var added = Set[Id]()
      cases.foreach(cs => {
        val (nc, cadd) = insertChecks(cs.body, mods)
        ncases = ncases :+ CaseObj(cs.cond, nc).setPos(cs.pos)
        added = added ++ cadd
      })
      val (ndef, nadd) = insertChecks(default, mods)
      (CSplit(ncases, ndef).setPos(c.pos), added ++ nadd)
    case CLockEnd(mod) if mods.contains(mod) =>
      val chk = CCheckpoint(checkHandle(mod), mod).setPos(c.pos)
      val condChk = checkpointConds(mod) match {
        case Some(value) => CIf(value, chk, CEmpty()).setPos(c.pos)
        case None => chk
      }
      (CSeq(condChk, c).setPos(c.pos), Set(mod))
    case _ => (c, Set())
  }

  //MEnv._1 == mems we're currently looking for
  //MEnv._2 == mems we're looking for next cycle
  type MEnv = (Set[Id], Set[Id])
  /**
   *
   * @param c
   * @param mods
   * @return
   */
  private def findInvalidateConds(c: Command, currentCond: Option[Expr], mems: MEnv): MEnv = c match {
    case CSeq(c1, c2) =>
      val lenv = findInvalidateConds(c1, currentCond, mems)
      findInvalidateConds(c2, currentCond, lenv)
    case CTBar(c1, c2) =>
      val lenv = findInvalidateConds(c1, currentCond, mems)
      findInvalidateConds(c2, currentCond, (lenv._1 ++ lenv._2, Set()))
    //TODO this copies some of the logic in PredicateGenerator (but that only produces Z3 info and we
    //don't have the logic to return from the abstractly interpreted Z3 conditions back into PDL Expressions
    case CIf(cond, cons, alt) =>
      val tenv = findInvalidateConds(cons, andExpr(Some(cond), currentCond), mems)
      val fenv = findInvalidateConds(alt, andExpr(Some(EUop(NotOp(), cond)), currentCond), mems)
      (tenv._1 ++ fenv._1, tenv._2 ++ fenv._2)
    case CSplit(cases, default) =>
      var res: MEnv = (Set(), Set())
      var lastCond: Option[Expr] = None
      cases.foreach(cs => {
        val nextCond = lastCond match {
          case Some(value) => AndOp(cs.cond, EUop(NotOp(), value))
          case None => cs.cond
        }
        lastCond = Some(nextCond)
        val nenv = findInvalidateConds(cs.body, lastCond, mems)
        res = (res._1 ++ nenv._1, res._2 ++ nenv._2)
      })
      val dEnv = findInvalidateConds(default, lastCond.map(e => EUop(NotOp(), e)), mems)
      (res._1 ++ dEnv._1, res._2 ++ dEnv._2)
    case CVerify(_, _, _, _, _) | CUpdate(_, _, _, _, _) | CInvalidate(_, _)=>
      //add to the condition for each module that needs checkpoint
      mems._1.foreach(m => {
        if (checkpointConds.contains(m)) {
          checkpointConds = checkpointConds + (m -> orExpr(checkpointConds(m), currentCond))
        } else {
          checkpointConds = checkpointConds + (m -> currentCond)
        }
      })
      mems
    case CLockEnd(mod) => (mems._1, mems._2 + mod) //start looking for mod the cycle after we see the lock end
    case _ => mems
  }

  private def haveCheckpoint(c: Command): Set[Id] = c match {
    case CSeq(c1, c2) => haveCheckpoint(c1) ++ haveCheckpoint(c2)
    case CTBar(c1, c2) => haveCheckpoint(c1) ++ haveCheckpoint(c2)
    case CIf(_, cons, alt) => haveCheckpoint(cons) ++ haveCheckpoint(alt)
    case CCheckpoint(_, lock) => Set(lock)
    case CSplit(cases, default) =>
      cases.foldLeft(haveCheckpoint(default))((s, cs) => {
        haveCheckpoint(cs.body) ++ s
      })
    case _ => Set()
  }

  //needs a checkpoint only if it actually does a reservation
  //TODO copies some logic from SpeculationChecker (when stages might possible be speculative)
  private def needsCheckpoint(c: Command, isMaybeSpec: Boolean): (Set[Id], Boolean) = c match {
    case CSeq(c1, c2) =>
      val (chk1, s1) = needsCheckpoint(c1, isMaybeSpec)
      val (chk2, s2) = needsCheckpoint(c2, s1)
      (chk1 ++ chk2, s2)
    case CTBar(c1, c2) =>
      val (chk1, s1) = needsCheckpoint(c1, isMaybeSpec)
      val (chk2, s2) = needsCheckpoint(c2, s1)
      (chk1 ++ chk2, s2)
    case CIf(_, cons, alt) =>
      val (chkt, s1) = needsCheckpoint(cons, isMaybeSpec)
      val (chkf, s2) = needsCheckpoint(alt, isMaybeSpec)
      (chkt ++ chkf, s1 || s2)
      //only need when we have _speculative_ reservations
    case CLockOp(mem, op, lockType, _, _) if isMaybeSpec && op == Reserved => (Set(mem.id), isMaybeSpec)
    case CSplit(cases, default) =>
      cases.foldLeft(needsCheckpoint(default, isMaybeSpec))((s, cs) => {
        val (chks, specStatus) = needsCheckpoint(cs.body, isMaybeSpec)
        (chks ++ s._1, specStatus || s._2) //or since we don't know which branch executed, so maybe spec if either maybe
      })
    case CCheckSpec(true) => (Set(), false)
    case _ => (Set(), isMaybeSpec)
  }
  /**
   *
   * @param c
   * @return
   */
  private def getResRegionIds(c: Command): Set[Id] = c match {
    case CSeq(c1, c2) => getResRegionIds(c1) ++ getResRegionIds(c2)
    case CTBar(c1, c2) => getResRegionIds(c1) ++ getResRegionIds(c2)
    case CIf(cond, cons, alt) => getAtomicAccessIds(cond) ++ getResRegionIds(cons) ++ getResRegionIds(alt)
    case CSplit(cases, default) => cases.foldLeft(getResRegionIds(default))((s, a) => {
      s ++ getResRegionIds(a.body) ++ getAtomicAccessIds(a.cond)
    })
    case _ => getImmediateResRegionIds(c)
  }

  /**
   *
   * @param c
   * @return
   */
  private def getImmediateResRegionIds(c: Command): Set[Id] = c match {
    case CAssign(lhs, rhs) => getAtomicAccessIds(lhs) ++ getAtomicAccessIds(rhs)
    case CRecv(lhs, rhs) => getAtomicAccessIds(lhs) ++ getAtomicAccessIds(rhs)
    case CSpecCall(_, _, args) => args.foldLeft(Set[Id]())((s, a) => s ++ getAtomicAccessIds(a))
    case CVerify(_, args, _, _, _) => args.foldLeft(Set[Id]())((s, a) => s ++ getAtomicAccessIds(a))
    case CUpdate(_, _, args, _, _) =>
      args.foldLeft(Set[Id]())((s, a) => s ++ getAtomicAccessIds(a))
    case CPrint(args) => args.foldLeft(Set[Id]())((s, a) => s ++ getAtomicAccessIds(a))
    case COutput(exp) => getAtomicAccessIds(exp)
    case CExpr(exp) => getAtomicAccessIds(exp)
    case CLockOp(mem, op, _, _, _) if op == Reserved => Set(mem.id)
    case _ => Set()
  }

  /**
   *
   * @param e
   * @return
   */
  private def getAtomicAccessIds(e: Expr): Set[Id] = e match {
    case EIsValid(ex) => getAtomicAccessIds(ex)
    case EFromMaybe(ex) => getAtomicAccessIds(ex)
    case EToMaybe(ex) => getAtomicAccessIds(ex)
    case EUop(_, ex) => getAtomicAccessIds(ex)
    case EBinop(_, e1, e2) => getAtomicAccessIds(e1) ++ getAtomicAccessIds(e2)
    case EMemAccess(mem, _, _, _, _, isAtomic) if isAtomic => Set(mem)
    case EMemAccess(mem, _, _, _, _, _) if unlockedMems.contains(mem) => Set(mem)
    case EBitExtract(num, _, _) => getAtomicAccessIds(num)
    case ETernary(cond, tval, fval) => getAtomicAccessIds(cond) ++ getAtomicAccessIds(tval) ++ getAtomicAccessIds(fval)
    case EApp(_, args) => args.foldLeft(Set[Id]())((b, a) => b ++ getAtomicAccessIds(a))
    case ECall(_, _, args, _) => args.foldLeft(Set[Id]())((b, a) => b ++ getAtomicAccessIds(a))
    case ECast(_, exp) => getAtomicAccessIds(exp)
    case _ => Set()
  }

  private def addLockStarts(ids: Set[Id], before: Command): Command = {
    ids.foldLeft(before)((c, i) => {
      CSeq(CLockStart(i).setPos(before.pos), c).setPos(before.pos)
    })
  }
  private def addLockEnds(ids: Set[Id], after: Command): Command = {
    ids.foldLeft(after)((c, i) => {
      CSeq(c, CLockEnd(i).setPos(after.pos)).setPos(after.pos)
    })
  }
}
