package pipedsl.passes

import pipedsl.common.Syntax._
import pipedsl.passes.Passes.{ModulePass, ProgPass}

class ExpandCatchPass extends ModulePass[ModuleDef] with ProgPass[Prog]{
  override def run(m: ModuleDef): ModuleDef =
  {
    if(is_excepting(m)){
      m.copy(body = expandCatch(m.body), commit_blk = m.commit_blk, except_blk = m.except_blk).copyMeta(m)
    } else {
      m
    }
  }
  override def run(p: Prog): Prog = p.copy(moddefs = p.moddefs.map(m => run(m).copyMeta(m)))

  def expandCatch(c: Command): Command = {
    c match {
      case CSeq(c1, c2) => CSeq(expandCatch(c1), expandCatch(c2)).copyMeta(c)
      case CTBar(c1, c2) => CTBar(expandCatch(c1), expandCatch(c2)).copyMeta(c)
      case CSplit(cases, default) =>
        val newCases = cases.map(c => CaseObj(c.cond, expandCatch(c.body)))
        CSplit(newCases, expandCatch(default)).copyMeta(c)
      case CCatch(mod, onCatch) => {
        val is_interrupt = EVar(Id("is_interrupt")).setPos(c.pos)
        is_interrupt.typ = Some(TBool())
        is_interrupt.id.typ = is_interrupt.typ
        val get_req = CAssign(is_interrupt, ECall(mod, Some(Id("req")), List[Expr](EVar(Id("pc"))), true))
        val ack = CExpr(ECall(mod, Some(Id("ack")), List[Expr](EVar(Id("pc"))), false))
        CSeq(get_req, CIf(is_interrupt, CSeq(ack, onCatch), CEmpty()))
      }
      case _ => c
    }
  }
}
