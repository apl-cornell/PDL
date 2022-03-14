package pipedsl.passes

import pipedsl.common.Errors.{ReleaseInExnBlock, ReleaseWhenMaybeExcepting}
import pipedsl.common.Locks.Released
import pipedsl.common.Syntax._

object ExceptingToNormal
 {
  def run(p :ExceptableProg) :Prog =
   {
    Prog(p.exts, p.fdefs, p.moddefs.map(translateModule), p.circ)
   }
  private def translateModule(m :ModuleTrait) :ModuleDef = m match
  {
   case m:ModuleDef => m
   case m@ExceptingModule(name, inputs, modules, ret, body, commit_block, exn_block) =>
    check_exn_block(exn_block)
    check_body(body)
    ModuleDef(
     name, inputs, modules, ret,
     ast_append(body, CIf(EVar(is_excepting_var), exn_block, commit_block))).copyMeta(m)
  }

  private def ast_append(original :Command, tail :Command) :Command = original match
  {
   case CTBar(c1, c2) => CTBar(c1, ast_append(c2, tail)).copyMeta(original)
   case CEmpty() => tail
   case _ => CSeq(original, tail)
  }

  private def check_body(c :Command) :Unit = c match
  {
   case CSeq(c1, c2) => check_body(c1); check_body(c2);
   case CTBar(c1, c2) => check_body(c1); check_body(c2);
   case CIf(_, cons, alt) => check_body(cons); check_body(alt)
   case CSplit(cases, default) => cases.foreach(c => check_body(c.body))
   case CLockOp(_, Released, _, _, _) => throw ReleaseWhenMaybeExcepting(c.pos)
   case _ => ()
  }

  private def check_exn_block(c :Command): Unit = c match
  {
   case CSeq(c1, c2) => check_exn_block(c1); check_exn_block(c2)
   case CTBar(c1, c2) => check_exn_block(c1); check_exn_block(c2);
   case CIf(_, cons, alt) => check_exn_block(cons); check_exn_block(alt)
   case CSplit(cases, default) => cases.foreach(c => check_exn_block(c.body)); check_exn_block(default)
   case CLockOp(_, Released, _, _, _) => throw ReleaseInExnBlock(c.pos)
   case _ => ()
  }


 }
