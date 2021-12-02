package pipedsl.passes

import pipedsl.common.Locks.Reserved
import pipedsl.common.Syntax._
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
  override def run(m: ModuleDef): ModuleDef = {
    unlockedMems = Set()
    m.modules.foreach(p => {
      p.typ match {
        case TMemType(elem, addrSize, readLatency, writeLatency, readPorts, writePorts) =>
          unlockedMems = unlockedMems + p.name
        case _ => ()
      }
    })
    val modIds = m.modules.map(p => p.name).toSet
    val starts = insertStarts(m.body, modIds, Set())._1
    val ends = insertEnds(starts, modIds, Set())._1
    val checks = insertChecks(ends, modIds)
    m.copy(body = checks).setPos(m.pos)
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

  private def insertChecks(c: Command, mods: Set[Id]): Command = c

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
    case ECall(_, _, args) => args.foldLeft(Set[Id]())((b, a) => b ++ getAtomicAccessIds(a))
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
