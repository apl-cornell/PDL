/* BindModuleTypes.scala */
package pipedsl.passes

import pipedsl.common.Syntax._
import pipedsl.passes.Passes.ProgPass
import pipedsl.typechecker.Environments.Environment

/**
 * This replaces any module parameters which currently have the type
 * NamedType with the type given by the provided type environment.
 * @param tenv The environment containing top level type mappings.
 */
class BindModuleTypes(val tenv: Environment[Id, Type]) extends ProgPass[Prog] {
  override def run(p: Prog): Prog = {
    p.copy(exts = p.exts, fdefs = p.fdefs, moddefs = p.moddefs.map(run))
  }

  def run(m: ModuleDef): ModuleDef = {
    m.copy(modules = m.modules.map(p => Param(p.name, replaceNamedType(p.typ)))).copyMeta(m)
  }

  private def replaceNamedType(t: Type): Type = t match {
    case TNamedType(name) => tenv(name)
    case _ => t
  }
}
