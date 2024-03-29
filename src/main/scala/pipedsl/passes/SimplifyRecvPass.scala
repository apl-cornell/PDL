package pipedsl.passes

import Passes.{CommandPass, ModulePass, ProgPass}
import pipedsl.common.Errors.UnexpectedCase
import pipedsl.common.Syntax._
import pipedsl.common.Utilities._

import scala.util.parsing.input.Position

/**
 * This pass converts RECV statements into a canonical form,
 * We convert the arguments to the operation into variables, instead of generic expressions.
 * This includes index expressions on the LHS of memory assignments, not just RHS expressions.
 */
object SimplifyRecvPass extends CommandPass[Command] with ModulePass[ModuleDef] with ProgPass[Prog] {

  var usedVars: Set[Id] = Set()
  var counter: Int = 0

  override def run(p: Prog): Prog = {
    usedVars = p.fdefs.foldLeft[Set[Id]](usedVars)((s,f) => s + f.name)
    p.copy(exts = p.exts, fdefs = p.fdefs, moddefs = p.moddefs.map(m => run(m))).setPos(p.pos)
  }

  override def run(m: ModuleDef): ModuleDef = {
    usedVars = m.modules.foldLeft[Set[Id]](usedVars)((s,p) => s + p.name)
    m.copy(body = run(m.body)).setPos(m.pos).copyMeta(m)
  }

  override def run(c: Command): Command = {
    usedVars = usedVars ++ getAllVarNames(c)
    runHelper(c)
  }

  private def newVar(s: String, p: Position, t: Option[Type]): EVar = {
    val (nvar, nused, ncnt) = freshVar(s, usedVars, counter)
    nvar.typ = t
    usedVars = nused
    counter = ncnt
    val v = EVar(nvar).setPos(p)
    v.typ = t
    v
  }

  private def runHelper(c:Command): Command = c match {
    case CSeq(c1, c2) => CSeq(runHelper(c1), runHelper(c2)).setPos(c.pos)
    case CTBar(c1, c2) => CTBar(runHelper(c1), runHelper(c2)).setPos(c.pos)
    case CSplit(cases, default) =>
      val newcases: List[CaseObj] = cases.foldLeft(List[CaseObj]())((l, cs) => {
        val nc = CaseObj(cs.cond, runHelper(cs.body))
        nc.setPos(cs.pos)
        l :+ nc
      })
      CSplit(newcases, runHelper(default).setPos(default.pos)).setPos(c.pos)
    case CIf(cond, cons, alt) => CIf(cond, runHelper(cons), runHelper(alt)).setPos(c.pos)
    case CRecv(lhs, rhs) => (lhs, rhs) match {
      case (EVar(_), EMemAccess(_, EVar(_),_, _, _, _)) => c //leave it alone, already in the form we want
      case (EVar(_), EMemAccess(mem, idx, wm, inHandle, outHandle, isAtomic)) => //separate out the index computation
        val idxAssgn = CAssign(newVar("index", idx.pos, idx.typ), idx).setPos(idx.pos)
        val access = EMemAccess(mem, idxAssgn.lhs, wm, inHandle, outHandle, isAtomic).setPos(rhs.pos)
        access.typ = rhs.typ
        access.portNum = rhs.portNum
        CSeq(idxAssgn, CRecv(lhs, access)).setPos(c.pos)
      case (EMemAccess(_, EVar(_), _, _, _, _), EVar(_)) => c //leave it alone, already in form we want
      case (EMemAccess(_,EVar(_), _, _, _, _), _) => // separate out the RHS computation
        val rhsAssgn = CAssign(newVar("msg", rhs.pos, rhs.typ), rhs).setPos(rhs.pos)
        CSeq(rhsAssgn, CRecv(lhs, rhsAssgn.lhs).setPos(c.pos)).setPos(c.pos)
      case (EMemAccess(mem,idx,wm, inHandle, outHandle, isAtomic), EVar(_)) => //separate out the index computation
        val idxAssgn = CAssign(newVar("index", idx.pos, idx.typ), idx).setPos(idx.pos)
        val access = EMemAccess(mem, idxAssgn.lhs, wm, inHandle, outHandle, isAtomic).setPos(lhs.pos)
        access.typ = lhs.typ
        access.portNum = lhs.portNum
        CSeq(idxAssgn, CRecv(access, rhs).setPos(c.pos)).setPos(c.pos)
      case (EMemAccess(mem,idx, wm, inHandle, outHandle, isAtomic), _) => //separate the index computation AND the rhs computation into new variables
        val idxAssgn = CAssign(newVar("index", idx.pos, idx.typ), idx).setPos(idx.pos)
        val rhsAssgn = CAssign(newVar("msg", rhs.pos, rhs.typ), rhs).setPos(rhs.pos)
        val access = EMemAccess(mem, idxAssgn.lhs, wm, inHandle, outHandle, isAtomic).setPos(lhs.pos)
        access.typ = lhs.typ
        access.portNum = lhs.portNum
        CSeq(idxAssgn,
          CSeq(rhsAssgn,
            CRecv(access, rhsAssgn.lhs).setPos(c.pos))
            .setPos(c.pos))
          .setPos(c.pos)
      case (EVar(_), ECall(id, name, args, isAtomic)) =>
        //TODO cleanup and stop copying code
        val argAssgns = args.foldLeft[(Command, List[Expr])]((CEmpty(), List()))((cs, a) => {
          val argAssn = CAssign(newVar("carg", a.pos, a.typ), a).setPos(a.pos)
          (CSeq(cs._1, argAssn).setPos(a.pos), cs._2 :+ argAssn.lhs)
        })
        CSeq(argAssgns._1, CRecv(lhs, ECall(id, name, argAssgns._2, isAtomic).setPos(c.pos)).setPos(c.pos)).setPos(c.pos)
      case (l@EVar(_), _) =>
        CAssign(l, rhs).setPos(c.pos)
      case _ => throw UnexpectedCase(c.pos)
    }
    //calls also get translated to send statements later
    case CExpr(ECall(id, name, args, isAtomic)) =>
      val argAssgns = args.foldLeft[(Command, List[Expr])]((CEmpty(), List()))((cs, a) => {
        val argAssn = CAssign(newVar("carg", a.pos, a.typ), a).setPos(a.pos)
        (CSeq(cs._1, argAssn).setPos(a.pos), cs._2 :+ argAssn.lhs)
      })
      CSeq(argAssgns._1, CExpr(ECall(id, name, argAssgns._2, isAtomic).setPos(c.pos)).setPos(c.pos)).setPos(c.pos)
    case _ => c
  }

}
