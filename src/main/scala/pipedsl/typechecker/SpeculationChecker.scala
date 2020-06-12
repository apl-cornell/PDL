package pipedsl.typechecker

import pipedsl.common.Syntax
import TypeChecker._
import Environments._

object SpeculationChecker extends TypeChecks[Boolean] {

  override def emptyEnv(): Environment[Boolean] = EmptyBoolEnv

  //No Speculation in Functions
  override def checkFunc(f: Syntax.FuncDef, env: Environment[Boolean]): Environment[Boolean] = env

  override def checkModule(m: Syntax.ModuleDef, env: Environment[Boolean]): Environment[Boolean] = {
    env
  }

  override def checkCircuit(c: Syntax.Circuit, env: Environment[Boolean]): Environment[Boolean] = env
}
