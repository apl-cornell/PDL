package pipedsl.passes

import pipedsl.common.Syntax._
import pipedsl.passes.Passes.{CommandPass, ModulePass, ProgPass}

/**
 * This pass propagates the checkpoint handles to
 * the commands that need to issue rollback or handle release commands in the code generation.
 * Specifically: Verify(), Invalidate(), and Update()
 * It _does not_ propagate them until a stage separator has passed -
 * the checkpoint isn't visible until the next stage and shouldn't issue a rollback concurrently with checkpointing.
 * If a TBar occurs in _either_ branch, then we will consider a stage passing (since this is how the
 * stage collapsing works right now).
 *
 * This pass does not check _anything_ a different pass is used to check the
 * well-formedness of checkpoint commands.
 */
object AddCheckpointHandlesPass extends CommandPass[Command] with ModulePass[ModuleDef] with ProgPass[Prog] {

  //CEnv._1 === handles available right now
  //CEnv._2 == handles available next cycle
  type CEnv = (Set[EVar], Set[EVar])

  override def run(c: Command): Command = {
    val (nc, _) = addCheckpointHandles(c, (Set(), Set()))
    nc
  }

  override def run(m: ModuleDef): ModuleDef = m.copy(body = run(m.body)).copyMeta(m)

  override def run(p: Prog): Prog = p.copy(exts = p.exts, fdefs = p.fdefs,
    moddefs = p.moddefs.map(m => run(m))).setPos(p.pos)


  private def addCheckpointHandles(c: Command, env: CEnv): (Command, CEnv) = c match {
    case CSeq(c1, c2) =>
      val (lc, lenv) = addCheckpointHandles(c1, env)
      val (rc, renv) = addCheckpointHandles(c2, lenv)
      ( CSeq(lc, rc).setPos(c.pos), renv )
    case CTBar(c1, c2) =>
      val (lc, lenv) = addCheckpointHandles(c1, env)
      val handleList = lenv._1 ++ lenv._2 //make handles available in next stage
      val (rc, renv) = addCheckpointHandles(c2, (handleList, Set()))
      ( CTBar(lc, rc).setPos(c.pos), renv )
    case CIf(cond, cons, alt) =>
      val (tc, tenv) = addCheckpointHandles(cons, env)
      val (fc, fenv) = addCheckpointHandles(alt, env)
      //just blindly merge, it's fine. We'll check that this
      //isn't nonsense in a different pass
      val nenv = (tenv._1 ++ fenv._1, tenv._2 ++ fenv._2)
      ( CIf(cond, tc, fc).setPos(c.pos), nenv )
    case CSplit(cases, default) =>
      val (dc, denv) = addCheckpointHandles(default, env)
      val ncases = cases.foldLeft(List[(CaseObj, CEnv)]())((e, c) => {
        val (nc, nenv) = addCheckpointHandles(c.body, env)
        val cobj = CaseObj(c.cond, nc).setPos(c.pos)
        e :+ (cobj, nenv)
      })
      val fenv = ncases.foldLeft[CEnv](denv)((s, c) => {
        (s._1 ++ c._2._1, s._2 ++ c._2._2)
      })
      ( CSplit(ncases.map(c => c._1), dc).setPos(c.pos), fenv )
    case CVerify(handle, args, preds, update, _) =>
      ( CVerify(handle, args, preds, update, env._1.toList).setPos(c.pos), env )
    case CUpdate(newHandle, handle, args, preds, _) =>
      ( CUpdate(newHandle, handle, args, preds, env._1.toList).setPos(c.pos), env )
    case CInvalidate(handle, _) =>
      ( CInvalidate(handle, env._1.toList).setPos(c.pos), env )
    case CCheckpoint(handle, _) =>
      (c, (env._1, env._2 + handle))
    case _ => (c, env)
  }
}
