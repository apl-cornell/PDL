package pipedsl.passes

import Passes.{CommandPass, ModulePass, ProgPass}
import pipedsl.common.Syntax._
import pipedsl.common.Utilities._
import scala.util.parsing.input.Position

object SimplifyRecvPass extends CommandPass[Command] with ModulePass[ModuleDef] with ProgPass[Prog] {

  var usedVars: Set[Id] = Set()
  var counter: Int = 0

  override def run(p: Prog): Prog = {
    p.copy(moddefs = p.moddefs.map(m => run(m))).setPos(p.pos)
  }

  override def run(m: ModuleDef): ModuleDef = {
    m.copy(body = run(m.body)).setPos(m.pos)
  }

  override def run(c: Command): Command = {
    usedVars = getAllVarNames(c)
    runHelper(c)
  }

  private def newVar(s: String, p: Position): EVar = {
    val (nvar, nused, ncnt) = freshVar(s, usedVars, counter)
    usedVars = nused
    counter = ncnt
    EVar(nvar).setPos(p)
  }

  private def runHelper(c:Command): Command = c match {
    case CSeq(c1, c2) => CSeq(runHelper(c1), runHelper(c2)).setPos(c.pos)
    case CTBar(c1, c2) => CTBar(runHelper(c1), runHelper(c2)).setPos(c.pos)
    case CIf(cond, cons, alt) => CIf(cond, runHelper(cons), runHelper(alt)).setPos(c.pos)
    case CRecv(lhs, rhs) => (lhs, rhs) match {
      case (EVar(_), EMemAccess(mem, idx)) => { //separate out the index computation
        val idxAssgn = CAssign(newVar("index", idx.pos), idx).setPos(idx.pos)
        CSeq(idxAssgn, CRecv(lhs, EMemAccess(mem, idxAssgn.lhs).setPos(rhs.pos))).setPos(c.pos)
      }
      case (EMemAccess(mem,idx), _) => { //separate the index computation AND the rhs computation into new variables
        val idxAssgn = CAssign(newVar("index", idx.pos), idx).setPos(idx.pos)
        val rhsAssgn = CAssign(newVar("msg", rhs.pos), rhs).setPos(rhs.pos)
        CSeq(idxAssgn,
          CSeq(rhsAssgn,
            CRecv(EMemAccess(mem, idxAssgn.lhs).setPos(lhs.pos), rhsAssgn.lhs).setPos(c.pos))
            .setPos(c.pos))
          .setPos(c.pos)
      }
    }
    //calls also get translated to send statements later
    case CCall(id, args) => {
      val argAssgns = args.foldLeft[(Command, List[Expr])]((CEmpty, List()))((cs, a) => {
        val argAssn = CAssign(newVar("carg", a.pos), a).setPos(a.pos)
        (CSeq(cs._1, argAssn).setPos(a.pos), cs._2 :+ argAssn.lhs)
      })
      CSeq(argAssgns._1, CCall(id, argAssgns._2).setPos(c.pos)).setPos(c.pos)
    }
    case _ => c
  }

}
