package pipedsl.passes

import pipedsl.common.Syntax.{CEmpty, CIf, CSeq, CSpeculate, CTBar, Command, ModuleDef, Prog}
import pipedsl.passes.Passes.{CommandPass, ModulePass, ProgPass}

/**
 * This pass puts the program into a canonical form. For now all this does is clean
 * up unnecessary and semantically meaningless statements, but maybe it will do more later.
 */
object CanonicalizePass extends CommandPass[Command] with ModulePass[ModuleDef] with ProgPass[Prog] {

  override def run(c: Command): Command = removeCEmpty(c)

  override def run(m: ModuleDef): ModuleDef = m.copy(body = run(m.body)).setPos(m.pos)

  override def run(p: Prog): Prog =  p.copy(moddefs = p.moddefs.map(m => run(m))).setPos(p.pos)


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
    case CSpeculate(predVar, predVal, verify, body) => CSpeculate(predVar, predVal, removeCEmpty(verify), removeCEmpty(body)).setPos(c.pos)
    case _ => c
  }
}
