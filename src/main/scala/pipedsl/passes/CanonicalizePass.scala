package pipedsl.passes

import pipedsl.common.Syntax
import pipedsl.common.Syntax._
import pipedsl.passes.Passes.{CommandPass, ModulePass, ProgPass}

/**
 * This pass puts the program into a canonical form. For now all this does is clean
 * up unnecessary and semantically meaningless statements, but maybe it will do more later.
 */
object CanonicalizePass extends CommandPass[Command] with ModulePass[ModuleDef] with ProgPass[Prog] {

  override def run(c: Command): Command = removeCEmpty(c)

  override def run(m: ModuleDef): ModuleDef = m.copy(body = run(m.body)).setPos(m.pos)

  override def run(p: Prog): Prog =
    p.copy(moddefs = p.moddefs.map(m => run((m))))
    .setPos(p.pos)


  /**
   * Removes all of the unnecessary CSeq commands that connect empty statements.
   * We can't do the same for CTBar since having a time barrier potentially has meaning, even if one
   * of the sides is empty.
   * @param c The command to clean up
   * @return The same command with no cseqs to/from empty commands
   */
  def removeCEmpty(c: Command): Command = c match {
    case CSeq(CEmpty(), CEmpty()) => CEmpty()
    case CSeq(c1, CEmpty()) => removeCEmpty(c1)
    case CSeq(CEmpty(), c2) => removeCEmpty(c2)
    case CSeq(c1, c2) => CSeq(removeCEmpty(c1), removeCEmpty(c2)).setPos(c.pos)
    case CTBar(c1, c2) => CTBar(removeCEmpty(c1), removeCEmpty(c2)).setPos(c.pos)
    case CIf(cond, cons, alt) => CIf(cond, removeCEmpty(cons), removeCEmpty(alt)).setPos(c.pos)
    case _ => c
  }

  /*this code was intended to do a limited CSE on combinational reads*/
  /*it turns out this is hard because of the way conditional expressions work*/
  /*so it is turned off for now. This would be better implemented later*/
  /*when we have the pipeline stages separated*/
  private var tmp_count = 0
  private def fresh_tmp() :EVar =
    { tmp_count += 1; EVar(Id("_iugwha_" + tmp_count))}


  def csemem_mod(moduleDef: ModuleDef): ModuleDef =
    moduleDef.copy(body =
      {val (bodp, _) = csemem_cmd(moduleDef.body, Map()); bodp})
      .setPos(moduleDef.pos)

  /**
   *
   * @param c
   * @param env is a mapping from memories and their indices to IDs
   * @return
   */
  def csemem_cmd(c: Command, env: Map[(Id, Id), Id])
  : (Command, Map[(Id, Id), Id]) = c match
  {
    case CSeq(c1, c2) =>
      val (c1p, nenv) = csemem_cmd(c1, env)
      val (c2p, fenv) = csemem_cmd(c2, nenv)
      (CSeq(c1p, c2p), fenv)
    case CTBar(c1, c2) =>
      val (c1p, nenv) = csemem_cmd(c1, env)
      val (c2p, fenv) = csemem_cmd(c2, nenv)
      (CTBar(c1p, c2p), fenv)
    case CIf(cond, cons, alt) =>
      val (defs, condp, nenv) = csemem_expr(cond, env)
      val (consp, _) = csemem_cmd(cons, env)
      val (altp, _) = csemem_cmd(alt, env)
      (CSeq(defs, CIf(condp, consp, altp)), nenv)
    case Syntax.CAssign(lhs@EVar(id), EMemAccess(mem, EVar(idx))) =>
      env.get((mem, idx)) match
      {
        case Some(mapped) => (CAssign(lhs, EVar(mapped)), env)
        case None => (c, env + ((mem, idx) -> id))
      }
    case Syntax.CAssign(lhs, rhs) =>
      val (defs, rhsp, nenv) = csemem_expr(rhs, env)
      (CSeq(defs, CAssign(lhs, rhsp)), nenv)
    case Syntax.CRecv(lhs@EVar(id), EMemAccess(mem, EVar(idx))) =>
      mem.typ.get match
      {
        case TLockedMemType(TMemType(_, _, readLatency, _), _, _) =>
          readLatency match
          {
            case pipedsl.common.Syntax.Latency.Combinational =>
              env.get((mem, idx)) match
              {
                case Some(mapped) => (CRecv(lhs, EVar(mapped)), env)
                case None => (c, env + ((mem, idx) -> id))
              }
            case _ => (c, env)
          }
      }
    case Syntax.CSpecCall(handle, pipe, args) =>
      val (fenv, defs, argsp) =
        args.foldLeft((env, CEmpty():Command, List.empty[Expr]))(
        (acc, arg) =>
          {
            val (cmd, argp, nenv) = csemem_expr(arg, acc._1)
            (nenv, CSeq(cmd, acc._2), argp::acc._3)
          })
      (CSeq(defs, CSpecCall(handle, pipe, argsp.reverse)), fenv)
    case Syntax.CVerify(handle, args, preds) =>
      val (fenv, defs, argsp) =
        args.foldLeft((env, CEmpty():Command, List.empty[Expr]))(
          (acc, arg) =>
            {
              val (cmd, argp, nenv) = csemem_expr(arg, acc._1)
              (nenv, CSeq(cmd, acc._2), argp::acc._3)
            })
      (CSeq(defs, CVerify(handle, argsp.reverse, preds)), fenv)
    case Syntax.COutput(exp) =>
      val (defs, expp, nenv) = csemem_expr(exp, env)
      (CSeq(defs, COutput(expp)), nenv)
    case Syntax.CReturn(exp) =>
      val (defs, expp, nenv) = csemem_expr(exp, env)
      (CSeq(defs, CReturn(expp)), nenv)
    case Syntax.CExpr(exp) =>
      val (defs, expp, nenv) = csemem_expr(exp, env)
      (CSeq(defs, CExpr(expp)), nenv)
    case Syntax.CSplit(cases, default) =>
      (CSplit(cases.map(cs =>
      {
        val (bodp, _) = csemem_cmd(cs.body, env)
        CaseObj(cs.cond, bodp)
      }), {val (defp, _) = csemem_cmd(default, env); defp}), env)
    case command: Syntax.InternalCommand => (c, env)
    case _ => (c, env)
  }

  def csemem_expr(e: Syntax.Expr, env: Map[(Id, Id), Id])
  : (Command, Syntax.Expr, Map[(Id, Id), Id]) = e match
  {
    case EIsValid(ex) =>
      val (defs, nexp, nenv) = csemem_expr(ex, env)
      (defs, EIsValid(nexp), nenv)
    case EFromMaybe(ex) =>
      val (defs, nexp, nenv) = csemem_expr(ex, env)
      (defs, EFromMaybe(nexp), nenv)
    case EToMaybe(ex) =>
      val (defs, nexp, nenv) = csemem_expr(ex, env)
      (defs, EToMaybe(nexp), nenv)
    case EUop(op, ex) =>
      val (defs, nexp, nenv) = csemem_expr(ex, env)
      (defs, EUop(op, nexp), nenv)
    case EBinop(op, e1, e2) =>
      val (defs1, ne1, nenv) = csemem_expr(e1, env)
      val (defs2, ne2, fenv) = csemem_expr(e2, nenv)
      (CSeq(defs1, defs2), EBinop(op, ne1, ne2), fenv)
    case ERecAccess(rec, fieldName) =>
      val (defs, nrec, nenv) = csemem_expr(rec, env)
      (defs, ERecAccess(nrec, fieldName), nenv)
    case EMemAccess(mem, EVar(idx)) => env.get((mem, idx)) match
    {
      case Some(id) => (CEmpty(), EVar(id), env)
      case None =>
        val frsh = fresh_tmp()
        val defs = CAssign(frsh, e)
        (defs, frsh, env + ((mem, idx) -> frsh.id))
    }
    case EBitExtract(num, start, end) =>
      val (defs, nump, nenv) = csemem_expr(num, env)
      (defs, EBitExtract(nump, start, end), nenv)
    case ETernary(cond, tval, fval) =>
      val (defs, condp, nenv) = csemem_expr(cond, env)
      (defs, ETernary(condp, tval, fval), nenv)
    case EApp(func, args) =>
      val (fenv, defs, argsp) =
        args.foldLeft((env, CEmpty():Command, List.empty[Expr]))(
          (acc, arg) =>
            {
              val (cmd, argp, nenv) = csemem_expr(arg, acc._1)
              (nenv, CSeq(cmd, acc._2), argp::acc._3)
            })
      (defs, EApp(func, argsp.reverse), fenv)
    case ECall(mod, args) =>
      val (fenv, defs, argsp) =
        args.foldLeft((env, CEmpty():Command, List.empty[Expr]))(
          (acc, arg) =>
            {
              val (cmd, argp, nenv) = csemem_expr(arg, acc._1)
              (nenv, CSeq(cmd, acc._2), argp::acc._3)
            })
      (defs, ECall(mod, argsp.reverse), fenv)
    case ECast(ctyp, exp) =>
      val (defs, nexp, nenv) = csemem_expr(exp, env)
      (defs, ECast(ctyp, nexp), nenv)
    case expr: CirExpr =>(CEmpty(), e, env)
    case _ => (CEmpty(), e, env)
  }

}
