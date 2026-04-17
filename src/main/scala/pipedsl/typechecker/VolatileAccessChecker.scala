package pipedsl.typechecker

import pipedsl.common.Syntax.*
import pipedsl.common.Errors.*

/**
 * Checks access rules for volatile memory types.
 *
 * Volatile memories are device registers (e.g., interrupt pending signal)
 * that may be modified by external hardware. Rules:
 *
 * 1. Volatile memories cannot be locked (they have no lock interface).
 * 2. Writes to volatile memory only allowed in final blocks (commit/except).
 * 3. Only one read and one write per instruction per volatile memory.
 *    (Multiple instructions can't simultaneously access the same volatile memory.)
 * 4. Reads only in non-speculative, in-order regions (including final blocks).
 *    (This is enforced by SpeculationChecker + TimingTypeChecker, not here.)
 */
object VolatileAccessChecker {

  def check(p: Prog): Unit =
    p.moddefs.foreach(checkModule)

  private def checkModule(m: ModuleDef): Unit = {
    m.except_blk match {
      case _: ExceptEmpty =>
        // Non-exception pipeline: just check no multiple accesses
        checkNoMultipleAccess(m.body, Set.empty, Set.empty)
      case ExceptFull(_, handler) =>
        // Exception pipeline:
        // - No volatile writes in body
        checkNoVolatileWriteInBody(m.body)
        // - Check multiple access across body + commit + except
        checkNoMultipleAccess(m.body, Set.empty, Set.empty)
        m.commit_blk.foreach(c => checkNoMultipleAccess(c, Set.empty, Set.empty))
        checkNoMultipleAccess(handler, Set.empty, Set.empty)
    }
  }

  /** No writes to volatile memory in the pipeline body */
  private def checkNoVolatileWriteInBody(c: Command): Unit = c match {
    case CSeq(c1, c2) =>
      checkNoVolatileWriteInBody(c1); checkNoVolatileWriteInBody(c2)
    case CTBar(c1, c2) =>
      checkNoVolatileWriteInBody(c1); checkNoVolatileWriteInBody(c2)
    case CIf(_, cons, alt) =>
      checkNoVolatileWriteInBody(cons); checkNoVolatileWriteInBody(alt)
    case CSplit(cases, default) =>
      checkNoVolatileWriteInBody(default)
      cases.foreach(co => checkNoVolatileWriteInBody(co.body))
    case CRecv(EMemAccess(mem, _, _, _, _, _), _) if isVolatileMemory(mem) =>
      throw IllegalVolatileWrite(c.pos)
    case _ => ()
  }

  /** Track reads and writes to volatile memories; error on duplicates */
  private def checkNoMultipleAccess(c: Command, reads: Set[Id], writes: Set[Id]): (Set[Id], Set[Id]) = c match {
    case CSeq(c1, c2) =>
      val (r1, w1) = checkNoMultipleAccess(c1, reads, writes)
      checkNoMultipleAccess(c2, r1, w1)
    case CTBar(c1, c2) =>
      val (r1, w1) = checkNoMultipleAccess(c1, reads, writes)
      checkNoMultipleAccess(c2, r1, w1)
    case CIf(_, cons, alt) =>
      val (r1, w1) = checkNoMultipleAccess(cons, reads, writes)
      val (r2, w2) = checkNoMultipleAccess(alt, reads, writes)
      (r1 ++ r2, w1 ++ w2)
    case CSplit(cases, default) =>
      val (rd, wd) = checkNoMultipleAccess(default, reads, writes)
      cases.foldLeft((rd, wd)) { case ((r, w), co) =>
        val (rc, wc) = checkNoMultipleAccess(co.body, reads, writes)
        (r ++ rc, w ++ wc)
      }
    // Write to volatile memory
    case CRecv(EMemAccess(mem, _, _, _, _, _), _) if isVolatileMemory(mem) =>
      if (writes.contains(mem)) throw NoMultipleVolatileAccess(c.pos)
      (reads, writes + mem)
    // Read from volatile memory
    case CRecv(_, EMemAccess(mem, _, _, _, _, _)) if isVolatileMemory(mem) =>
      if (reads.contains(mem)) throw NoMultipleVolatileAccess(c.pos)
      (reads + mem, writes)
    case _ => (reads, writes)
  }
}
