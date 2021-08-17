package pipedsl.passes

import pipedsl.common.Errors.MissingPredictionValues
import pipedsl.common.Syntax._
import pipedsl.passes.Passes.{CommandPass, ModulePass, ProgPass}

object AddVerifyValuesPass extends CommandPass[Command] with ModulePass[ModuleDef] with ProgPass[Prog] {

  type VEnv = Map[Id, List[EVar]];

  override def run(c: Command): Command = {
    val (nc, _) = addVerifyValues(c, Map())
    nc
  }

  override def run(m: ModuleDef): ModuleDef = m.copy(body = run(m.body)).setPos(m.pos)

  override def run(p: Prog): Prog =  p.copy(moddefs = p.moddefs.map(m => run(m))).setPos(p.pos)

  private def addVerifyValues(c: Command, env: VEnv): (Command, VEnv) = c match {
    case CSeq(c1, c2) =>
      val (lc, lenv) = addVerifyValues(c1, env)
      val (rc, renv) = addVerifyValues(c2, lenv)
      ( CSeq(lc, rc).setPos(c.pos), renv )
    case CTBar(c1, c2) =>
      val (lc, lenv) = addVerifyValues(c1, env)
      val (rc, renv) = addVerifyValues(c2, lenv)
      ( CTBar(lc, rc).setPos(c.pos), renv )
    case CIf(cond, cons, alt) =>
      val (lc, lenv) = addVerifyValues(cons, env)
      val (rc, renv) = addVerifyValues(alt, env)
      ( CIf(cond, lc, rc).setPos(c.pos), intersectEnv(lenv, renv) )
    case CSpecCall(handle, _, args) =>
      //For each arg, extract it into a variable (_handle_idx = args[idx])
      //Then add the mapping from the speculation handle to the list of args
      val assgnCmds = args.zipWithIndex.foldLeft[(Command, List[EVar])](CEmpty(), List[EVar]())((cm, a) => {
        val arg = a._1
        val idx = a._2
        val assgn = makeArgVariable(handle.id, idx, arg)
        val priorCmds = cm._1
        val args = cm._2
        ( CSeq(priorCmds, assgn).setPos(c.pos), args :+ assgn.lhs )
      })
      (CSeq(assgnCmds._1, c), env + (handle.id -> assgnCmds._2))
    case CVerify(handle, args, preds) =>
      if (!env.contains(handle.id)) {
        throw MissingPredictionValues(c.pos, handle.id.v)
      }
      val nc = CVerify(handle, args, env(handle.id)).setPos(c.pos)
      (nc, env)
    case CSplit(cases, default) =>
      val venvs = cases.foldLeft(List[(CaseObj, VEnv)]())((l, b) => {
        val (nc, nenv) = addVerifyValues(b.body, env)
        val nobj = CaseObj(b.cond, nc).setPos(b.pos)
        l :+ (nobj, nenv)
      })
      val (defCmd, defEnv) = addVerifyValues(default, env)
      val fenv = venvs.foldLeft(defEnv)((e, b) => {
        intersectEnv(e, b._2)
      })
      val nc = CSplit(venvs.map(c => c._1), defCmd)
      (nc, fenv)
    case _ => (c, env)
  }

  private def makeArgVariable(handle: Id, index: Integer, arg: Expr): CAssign = {
    val varId = Id("_" + handle.v + "_" + index).setPos(handle.pos)
    val aVar = EVar(varId).setPos(handle.pos)
    CAssign(aVar, arg).setPos(handle.pos)
  }

  //if the values are defined in either branch, take those.
  //if defined in both branches, then only add if they match
  private def intersectEnv(e1: VEnv, e2: VEnv): VEnv = {
    val keys = e1.keySet.union(e2.keySet)
    keys.foldLeft[VEnv](Map())((m, k) => {
      val l = e1.get(k)
      val r = e2.get(k)
      (l, r) match {
        case (Some(lv), Some(rv)) if lv == rv => m + (k -> lv)
        case (Some(lv), Some(rv)) => m //TODO, generate a set of conditional expressions
        case (None, Some(rv)) => m + (k -> rv) //use only value if only 1 branch defines
        case (Some(lv), None) => m + (k -> lv)
        case (None, None) => m
      }
    })
  }
}
