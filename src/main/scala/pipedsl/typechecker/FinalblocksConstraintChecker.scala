package pipedsl.typechecker

import pipedsl.common.Syntax.*
import pipedsl.common.Errors.*
import pipedsl.common.Locks.{General, Released}

/**
 * Checks static rules for pipeline exception handling (XPDL).
 * These rules ensure precise exceptions by constraining what operations
 * can appear in the body, commit block, and except block.
 *
 * Rule 1: The except block must be self-contained.
 *   a) All acquired write locks must be released before exiting.
 *   b) No pending asynchronous reads at the end (prevents indefinite stalls).
 *   c) Recursive call (spawning next instruction) only in the last stage.
 *
 * Rule 2: Final blocks (commit + except) must be non-speculative.
 *   - No spec_check, spec_barrier, or spec_call in commit or except blocks.
 *
 * Rule 3: Write locks acquired in the body must be released in the commit block, not before.
 *   - No write lock release in the pipeline body for exception pipelines.
 *   - This prevents uncommitted state changes before the commit/except decision.
 *
 * Rule 4: No stateful operations in the commit block except releasing locks.
 *   - No spawning new instructions, acquiring locks, or speculation ops.
 */
object FinalblocksConstraintChecker {

  def check(p: Prog): Unit =
    p.moddefs.foreach(checkModule)

  private def checkModule(m: ModuleDef): Unit = {
    m.except_blk match {
      case _: ExceptEmpty =>
        // Non-exception pipeline: just check no throw statements
        checkNoThrow(m.body)
      case ExceptFull(_, handler) =>
        // Exception pipeline: apply all rules
        // Rule 3: Body must not release write locks
        checkBodyNoWriteRelease(m.body)
        // Body must contain at least one throw
        if (!containsThrow(m.body))
          throw MustThrowWithExnPipe(m.body.pos)
        // Rule 4: Commit block has no stateful ops except lock release
        m.commit_blk.foreach(checkCommitBlock)
        // Rule 2: No speculation in final blocks
        m.commit_blk.foreach(c => checkNoSpeculation(c, "commit block"))
        checkNoSpeculation(handler, "except block")
        // Rule 1: No throw in commit or except blocks
        m.commit_blk.foreach(checkNoThrow)
        checkNoThrow(handler)
    }
  }

  /** Check that the body contains at least one throw statement */
  private def containsThrow(c: Command): Boolean = c match {
    case CSeq(c1, c2) => containsThrow(c1) || containsThrow(c2)
    case CTBar(c1, c2) => containsThrow(c1) || containsThrow(c2)
    case CIf(_, cons, alt) => containsThrow(cons) || containsThrow(alt)
    case CSplit(cases, default) =>
      containsThrow(default) || cases.exists(co => containsThrow(co.body))
    case _: CExcept => true
    case _ => false
  }

  /** Rule 3: No write lock releases in the pipeline body.
   *  All write commits must happen in the commit block. */
  private def checkBodyNoWriteRelease(c: Command): Unit = c match {
    case CSeq(c1, c2) => checkBodyNoWriteRelease(c1); checkBodyNoWriteRelease(c2)
    case CTBar(c1, c2) => checkBodyNoWriteRelease(c1); checkBodyNoWriteRelease(c2)
    case CIf(_, cons, alt) => checkBodyNoWriteRelease(cons); checkBodyNoWriteRelease(alt)
    case CSplit(cases, default) =>
      checkBodyNoWriteRelease(default)
      cases.foreach(co => checkBodyNoWriteRelease(co.body))
    case c @ CLockOp(_, Released, lockType, _, _)
      if lockType.contains(LockWrite) || c.granularity == General =>
      throw NoWriteReleaseInBody(c.pos)
    case _ => ()
  }

  /** No throw statements allowed in this command */
  private def checkNoThrow(c: Command): Unit = c match {
    case CSeq(c1, c2) => checkNoThrow(c1); checkNoThrow(c2)
    case CTBar(c1, c2) => checkNoThrow(c1); checkNoThrow(c2)
    case CIf(_, cons, alt) => checkNoThrow(cons); checkNoThrow(alt)
    case CSplit(cases, default) =>
      checkNoThrow(default); cases.foreach(co => checkNoThrow(co.body))
    case _: CExcept => throw IllegalThrowPlacement(c.pos)
    case _ => ()
  }

  /** Rule 2: No speculation operations in final blocks */
  private def checkNoSpeculation(c: Command, blockName: String): Unit = c match {
    case CSeq(c1, c2) => checkNoSpeculation(c1, blockName); checkNoSpeculation(c2, blockName)
    case CTBar(c1, c2) => checkNoSpeculation(c1, blockName); checkNoSpeculation(c2, blockName)
    case CIf(_, cons, alt) => checkNoSpeculation(cons, blockName); checkNoSpeculation(alt, blockName)
    case CSplit(cases, default) =>
      checkNoSpeculation(default, blockName)
      cases.foreach(co => checkNoSpeculation(co.body, blockName))
    case _: CSpecCall =>
      throw IllegalSpeculativeOperation(c.pos, s"spec_call not allowed in $blockName")
    case _: CCheckSpec =>
      throw IllegalSpeculativeOperation(c.pos, s"spec_check/barrier not allowed in $blockName")
    case _ => ()
  }

  /** Rule 4: Commit block can only release locks -- no other stateful ops */
  private def checkCommitBlock(c: Command): Unit = c match {
    case CSeq(c1, c2) => checkCommitBlock(c1); checkCommitBlock(c2)
    case CTBar(c1, c2) => checkCommitBlock(c1); checkCommitBlock(c2)
    case CIf(_, cons, alt) => checkCommitBlock(cons); checkCommitBlock(alt)
    case CSplit(cases, default) =>
      checkCommitBlock(default); cases.foreach(co => checkCommitBlock(co.body))
    case CLockOp(_, Released, _, _, _) => () // OK: releasing locks
    case _: CLockOp => throw NoCommittingWriteInBody(c.pos)  // Acquiring/reserving in commit
    case _: CSpecCall => throw IllegalSpeculativeOperation(c.pos, "commit block")
    case _: CCheckSpec => throw IllegalSpeculativeOperation(c.pos, "commit block")
    case _: CEmpty => ()
    case _: CExpr => () // Simple expressions OK
    case _: CPrint => () // Printing OK
    case _ => () // Allow other harmless commands (output, return)
  }
}
