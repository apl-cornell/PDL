package pipedsl.passes

import pipedsl.common.Syntax.*
import pipedsl.passes.Passes.*

/**
 * Translates exception syntax (throw/commit/except) into internal commands.
 *
 * Translation rules (from XPDL paper Section 3.3):
 *
 * 1. throw(args) -> lef = true; earg_0 = args[0]; ...; earg_n = args[n]
 *    (lef = local exception flag, earg_i = exception argument variables)
 *
 * 2. At each stage boundary in the body: if (gef) skip
 *    (gef = global exception flag, prevents later stages from executing)
 *
 * 3. At pipeline end:
 *    if (lef) { except_path } else { commit_path }
 *
 * 4. Except path:
 *    gef = true;
 *    --- (padding stages for preceding commits to finish)
 *    pipeclear; specclear; abort(M1); ... abort(Mn);
 *    --- except_block_body;
 *    gef = false;
 *
 * This pass runs AFTER type checking and BEFORE stage splitting.
 * It only transforms modules that have exception blocks.
 */
object ExnTranslationPass extends ModulePass[ModuleDef] {

  private val lefId = Id("__lef")  // local exception flag
  private def exnArgId(i: Int): Id = Id(s"__exn_arg_$i")

  override def run(m: ModuleDef): ModuleDef = {
    if (!m.hasExceptions) return m

    val ExceptFull(exnArgs, handler) = m.except_blk: @unchecked

    // Build the translated body
    val translatedBody = translateBody(m.body, exnArgs)

    // Build the commit path (just the commit block commands)
    val commitPath = m.commit_blk.getOrElse(CEmpty())

    // Build the except path:
    // gef = true; --- padding; pipeclear; specclear; abort(memories); --- handler; gef = false
    val memIds = m.modules.filter(p => p.typ match {
      case _: TLockedMemType | _: TMemType => true
      case _ => false
    }).map(_.name)

    val abortCmds = memIds.foldLeft[Command](CEmpty()) { (acc, mem) =>
      CSeq(acc, IAbort(mem))
    }

    val exceptPath = CSeq(
      ISetGlobalExnFlag(true),
      CSeq(
        CTBar(CEmpty(), CSeq(  // Stage separator for padding
          IFifoClear(),
          CSeq(ISpecClear(), abortCmds)
        )),
        CSeq(
          CTBar(CEmpty(), handler),  // Handler in its own stage(s)
          ISetGlobalExnFlag(false)
        )
      )
    )

    // Final block: if (lef) except else commit
    val lefVar = EVar(lefId)
    lefVar.typ = Some(TBool())
    val finalBlock = CIf(lefVar, exceptPath, commitPath)

    // Inject ICheckExn at each stage boundary in the body
    val bodyWithExnCheck = injectExnChecks(translatedBody)

    // Combine: body + final block
    val fullBody = CSeq(bodyWithExnCheck, finalBlock)

    m.copy(
      body = fullBody,
      commit_blk = None,        // Absorbed into the translated body
      except_blk = ExceptEmpty() // Absorbed into the translated body
    ).copyMeta(m)
  }

  /** Translate throw statements into lef assignments */
  private def translateBody(c: Command, exnArgs: List[Id]): Command = c match {
    case CSeq(c1, c2) => CSeq(translateBody(c1, exnArgs), translateBody(c2, exnArgs))
    case CTBar(c1, c2) => CTBar(translateBody(c1, exnArgs), translateBody(c2, exnArgs))
    case CIf(cond, cons, alt) => CIf(cond, translateBody(cons, exnArgs), translateBody(alt, exnArgs))
    case CSplit(cases, default) =>
      CSplit(cases.map(co => CaseObj(co.cond, translateBody(co.body, exnArgs))), translateBody(default, exnArgs))
    case CExcept(args) =>
      // throw(args) -> lef = true; earg_0 = args[0]; ...
      val lefAssign = CAssign(EVar(lefId), EBool(true))
      val argAssigns = args.zipWithIndex.foldLeft[Command](lefAssign) { case (acc, (arg, i)) =>
        val target = EVar(exnArgId(i))
        target.typ = exnArgs.lift(i).flatMap(_.typ)
        CSeq(acc, CAssign(target, arg))
      }
      argAssigns
    case _ => c
  }

  /** Inject ICheckExn after each stage separator in the body */
  private def injectExnChecks(c: Command): Command = c match {
    case CTBar(c1, c2) =>
      // After stage separator, check gef before executing the next stage
      CTBar(injectExnChecks(c1), CSeq(ICheckExn(), injectExnChecks(c2)))
    case CSeq(c1, c2) => CSeq(injectExnChecks(c1), injectExnChecks(c2))
    case CIf(cond, cons, alt) => CIf(cond, injectExnChecks(cons), injectExnChecks(alt))
    case CSplit(cases, default) =>
      CSplit(cases.map(co => CaseObj(co.cond, injectExnChecks(co.body))), injectExnChecks(default))
    case _ => c
  }
}
