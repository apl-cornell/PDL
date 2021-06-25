package pipedsl.passes

import pipedsl.common.Syntax._
import pipedsl.common.Utilities.getAllVarNames
import pipedsl.passes.Passes.{CommandPass, FunctionPass, ModulePass, ProgPass}

/**
 * This pass puts the program into a canonical form. For now all this does is clean
 * up unnecessary and semantically meaningless statements, but maybe it will do more later.
 *
 * Now it also lifts all cast expressions into temporary variables to simplify code generation
 * and remove problems with implicit casting in the generated code.
 */
class CanonicalizePass() extends CommandPass[Command] with ModulePass[ModuleDef]
  with FunctionPass[FuncDef] with ProgPass[Prog] {

  var usedNames: Set[String] = Set()
  var counter = 0

  private def freshTmp(e: Expr): CAssign = {
    var name = "_tmp_" + counter
    while (usedNames.contains(name)) {
      counter += 1
      name = "_tmp_" + counter
    }
    usedNames = usedNames + name
    val evar = EVar(Id(name).setPos(e.pos)).setPos(e.pos)
    evar.typ = e.typ
    CAssign(evar, e).setPos(e.pos)
  }

  override def run(c: Command): Command = {
    usedNames = getAllVarNames(c).map(i => i.v)
    val nc = extractCastVars(c)
    removeCEmpty(nc)
  }

  override def run(m: ModuleDef): ModuleDef = m.copy(body = run(m.body)).setPos(m.pos)

  override def run(f: FuncDef): FuncDef = f.copy(body = run(f.body)).setPos(f.pos)

  override def run(p: Prog): Prog = p.copy(moddefs = p.moddefs.map(m => run(m)),
    fdefs = p.fdefs.map(f => run(f))).setPos(p.pos)



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

  def extractCastVars(c: Command): Command = c match {
    case CSeq(c1, c2) => CSeq(extractCastVars(c1), extractCastVars(c2)).setPos(c.pos)
    case CTBar(c1, c2) => CTBar(extractCastVars(c1), extractCastVars(c2)).setPos(c.pos)
    case CIf(cond, cons, alt) =>
      val (ncond, nassgns) = extractCastVars(cond)
      val nif = CIf(ncond, extractCastVars(cons), extractCastVars(alt)).setPos(c.pos)
      CSeq(nassgns, nif).setPos(c.pos)
    case CSplit(cases, default) =>
      val ndef = extractCastVars(default)
      var assngs: Command = CEmpty()
      val ncases = cases.foldLeft(List[CaseObj]())((l, cobj) => {
        val (ncond, nassgns)  = extractCastVars(cobj.cond)
        val nbody = extractCastVars(cobj.body)
        assngs = CSeq(assngs, nassgns).setPos(c.pos)
        l :+ CaseObj(ncond, nbody).setPos(cobj.pos)
      })
      CSeq(assngs, CSplit(ncases, ndef).setPos(c.pos)).setPos(c.pos)
    case CAssign(lhs, rhs) =>
      val (nrhs, nassgns) = extractCastVars(rhs)
      val nc = CAssign(lhs, nrhs).setPos(c.pos)
      CSeq(nassgns, nc).setPos(c.pos)
    case CRecv(lhs, rhs) =>
      val (nrhs, na1) = extractCastVars(rhs)
      val (nlhs, na2) = extractCastVars(lhs)
      val nassgns = CSeq(na1, na2).setPos(c.pos)
      CSeq(nassgns, CRecv(nlhs, nrhs).setPos(c.pos)).setPos(c.pos)
    case CSpecCall(handle, pipe, args) =>
      val (nargs, nc) = extractCastVars(args)
      CSeq(nc, CSpecCall(handle, pipe, nargs).setPos(c.pos)).setPos(c.pos)
    case CCheckSpec(_) => c
    case CVerify(handle, args, preds) =>
      val (nargs, nc) = extractCastVars(args)
      val (npreds, nc2) = extractCastVars(preds)
      CSeq(nc, CSeq(nc2, CVerify(handle, nargs, npreds)
        .setPos(c.pos)).setPos(c.pos)).setPos(c.pos)
    case CInvalidate(_) => c
    case CPrint(_) => c
    case COutput(exp) =>
      val (nexp, nasgn) = extractCastVars(exp)
      CSeq(nasgn, COutput(nexp).setPos(c.pos)).setPos(c.pos)
    case CReturn(exp) =>
      val (nexp, nasgn) = extractCastVars(exp)
      CSeq(nasgn, CReturn(nexp).setPos(c.pos)).setPos(c.pos)
    case CExpr(exp) =>
      val (nexp, nasgn) = extractCastVars(exp)
      CSeq(nasgn, CExpr(nexp).setPos(c.pos)).setPos(c.pos)
    case CLockStart(_) => c
    case CLockEnd(_) => c
    case CLockOp(_, _, _) => c
    case CEmpty() => c
    case _: InternalCommand => c
  }

  def extractCastVars(es: Iterable[Expr]): (List[Expr], Command) = {
    es.foldLeft[(List[Expr],Command)]((List[Expr](), CEmpty()))((t, e) => {
      val l = t._1
      val c = t._2
      val (nl, nc) = extractCastVars(e)
      (l :+ nl, CSeq(c, nc).setPos(e.pos))
    })
  }

  /**
   * Extract any subexpressions which are casts
   * and return assignments to them as new variables.
   * Also return a new expression which references the new variables.
   * @param e
   * @return
   */
  def extractCastVars(e: Expr): (Expr, Command) = e match {
    case EIsValid(ex) =>
      val (ne, nc) = extractCastVars(ex)
      (EIsValid(ne).setPos(e.pos), nc)
    case EFromMaybe(ex) =>
      val (ne, nc) = extractCastVars(ex)
      (EFromMaybe(ne).setPos(e.pos), nc)
    case EToMaybe(ex) =>
      val (ne, nc) = extractCastVars(ex)
      (EToMaybe(ne).setPos(e.pos), nc)
    case EUop(op, ex) =>
      val (ne, nc) = extractCastVars(ex)
      (EUop(op, ne).setPos(e.pos), nc)
    case EBinop(op, e1, e2) =>
      val (ne1, nc1) = extractCastVars(e1)
      val (ne2, nc2) = extractCastVars(e2)
      (EBinop(op, ne1, ne2).setPos(e.pos), CSeq(nc1, nc2).setPos(nc1.pos))
    case EMemAccess(mem, index, Some(mask)) =>
      val (ne, nc) = extractCastVars(index)
      val (nm, ncm) = extractCastVars(mask)
      (EMemAccess(mem, ne, Some(nm)).setPos(e.pos),
        CSeq(nc, ncm).setPos(e.pos))
    case EMemAccess(mem, index, None) =>
      val (ne, nc) = extractCastVars(index)
      (EMemAccess(mem, ne, None).setPos(e.pos), nc)
    case EBitExtract(num, start, end) =>
      val (ne, nc) = extractCastVars(num)
      (EBitExtract(ne, start, end).setPos(e.pos), nc)
    case ETernary(cond, tval, fval) =>
      val (ncond, nc) = extractCastVars(cond)
      val (net, nct) = extractCastVars(tval)
      val (nef, ncf) = extractCastVars(fval)
      (ETernary(ncond, net, nef).setPos(e.pos),
        CSeq(CSeq(nc, nct).setPos(e.pos), ncf).setPos(e.pos))
    case EApp(func, args) =>
      val (nargs, nc) = extractCastVars(args)
      (EApp(func, nargs).setPos(e.pos), nc)
    case ECall(mod, args) =>
      val (nargs, nc) = extractCastVars(args)
      (ECall(mod, nargs).setPos(e.pos), nc)
    case ECast(ctyp, e) =>
      val (ne, nc) = extractCastVars(e)
      val ncast = ECast(ctyp, ne)
      ncast.typ = Some(ctyp)
      val nassgn = freshTmp(ncast)
      (nassgn.lhs, CSeq(nc, nassgn).setPos(e.pos))
    case _ => (e, CEmpty())
  }
}
