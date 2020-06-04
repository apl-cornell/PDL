package pipedsl.typechecker

import pipedsl.common.Syntax._
import Environments.TypeEnvironment

object TypeChecker {

  trait TypeChecks {

    def check(p: Prog, env: Option[TypeEnvironment]): TypeEnvironment = {
      val Prog(fdefs, mdefs, cir) = p
      val senv = env match { case Some(e) => e; case None => Environments.EmptyEnv }
      val fenv = fdefs.foldLeft[TypeEnvironment](senv)((tenv, fdef) => {
        checkFunc(fdef, tenv)
      })
      val menv = mdefs.foldLeft[TypeEnvironment](fenv)((tenv, mdef) => {
        checkModule(mdef, tenv)
      })
      checkCircuit(cir, menv)
    }

    def checkFunc(f: FuncDef, env:TypeEnvironment): TypeEnvironment

    def checkModule(m: ModuleDef, env: TypeEnvironment): TypeEnvironment

    def checkCircuit(c: Circuit, env: TypeEnvironment): TypeEnvironment
  }
}
