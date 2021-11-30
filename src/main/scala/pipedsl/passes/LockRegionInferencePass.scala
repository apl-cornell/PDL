package pipedsl.passes

import com.microsoft.z3.{AST => Z3AST, BoolExpr => Z3BoolExpr, Context => Z3Context, Solver => Z3Solver, Status => Z3Status}
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
class LockRegionInferencePass(val ctx: Z3Context)
  extends ModulePass[ModuleDef] with ProgPass[Prog] {

  override def run(p: Prog): Prog = p.copy(exts = p.exts, fdefs = p.fdefs,
    moddefs = p.moddefs.map(m => run(m))).setPos(p.pos)

  override def run(m: ModuleDef): ModuleDef = {
    val modIds = m.modules.map(p => p.name)
    val starts = insertStarts(m.body, modIds.toSet)
    val ends = insertEnds(starts, modIds)
    val checks = insertChecks(ends, modIds)
    m.copy(body = checks).setPos(m.pos)
  }

  //TODO this is very wrong
  private def insertStarts(c: Command, mods: Set[Id]): Command = c match {
    case CSeq(c1, c2) =>
      val addLhsIds = getImmediateResRegionIds(c1).intersect(mods)
      val addRhsIds = getImmediateResRegionIds(c2).intersect(mods)
      if (addLhsIds.nonEmpty) {
        CSeq(addLockStarts(addLhsIds, c1), insertStarts(c2, mods -- addLhsIds)).setPos(c.pos)
      } else if (addRhsIds.nonEmpty) {
        CSeq(c1, )
      } else {
        c
      }
      //replace with CSeq(CSeq(startlist, c1), insertStarts(c2, mods - startlist)))
    case CSeq(c1, c2) if getImmediateResRegionIds(c2).nonEmpty =>
      //replace with CSeq(c1, CSeq(start, c2))
    case CTBar(c1, c2) if getImmediateResRegionIds(c1).nonEmpty =>
    //replace with CTBar(CSeq(start, c1), c2)
    case CTBar(c1, c2) if getImmediateResRegionIds(c1).nonEmpty =>
    //replace with CTBar(c1, CSeq(start, c2))
    case CIf(cond, cons, alt) =>
      //if ID in cond OR anywhere in cons & alt
      //replace with CSeq(start, c)
      //else if id anywhere in cons
      //replace with CIf(cond, insertStarts(cons, mods), alt)
      //else
      //replace with CIf(cond, cons, insertStarts(alt, mods))
    case CSplit(cases, default) =>
    case _ => c
  }

  private def insertEnds(command: Command, value: List[Id]): Command = ???

  private def insertChecks(value: Command, value1: List[Id]): Command = ???

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
    case CSpecCall(_, pipe, args) => args.foldLeft(Set[Id]())((s, a) => s ++ getAtomicAccessIds(a))
    case CVerify(_, args, _, _, _) => args.foldLeft(Set[Id]())((s, a) => s ++ getAtomicAccessIds(a))
    case CUpdate(_, _, args, _, _) =>
      args.foldLeft(Set[Id]())((s, a) => s ++ getAtomicAccessIds(a))
    case CPrint(args) => args.foldLeft(Set[Id]())((s, a) => s ++ getAtomicAccessIds(a))
    case COutput(exp) => getAtomicAccessIds(exp)
    case CExpr(exp) => getAtomicAccessIds(exp)
    case CLockOp(mem, op, _, _, _) if op == Reserved => Set(mem)
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
    case EBitExtract(num, _, -) => getAtomicAccessIds(num)
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
}
