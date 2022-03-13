package pipedsl.passes

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
   case ExceptingModule(name, inputs, modules, ret, body, commit_block, exn_block) => ???
  }


 }
