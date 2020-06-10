package pipedsl.passes

import Passes.CommandPass
import pipedsl.common.Syntax._
import pipedsl.common.Utilities._
import scala.util.parsing.input.Position

object SimplifyRecvPass extends CommandPass[Command] {

  var usedVars: Set[Id] = Set()
  var counter: Int = 0

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
    case CCall(id, args) => {
      
    }
    case _ => c
  }
}
