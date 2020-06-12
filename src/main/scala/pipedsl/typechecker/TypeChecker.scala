package pipedsl.typechecker

import pipedsl.common.Syntax._
import Environments.Environment

object TypeChecker {

  trait TypeChecks[T] {

    def emptyEnv(): Environment[T]

    def check(p: Prog, env: Option[Environment[T]]): Environment[T] = {
      val Prog(fdefs, mdefs, cir) = p
      val senv = env match { case Some(e) => e; case None => emptyEnv() }
      val fenv = fdefs.foldLeft[Environment[T]](senv)((tenv, fdef) => {
        checkFunc(fdef, tenv)
      })
      val menv = mdefs.foldLeft[Environment[T]](fenv)((tenv, mdef) => {
        checkModule(mdef, tenv)
      })
      checkCircuit(cir, menv)
    }

    def checkFunc(f: FuncDef, env:Environment[T]): Environment[T]

    def checkModule(m: ModuleDef, env: Environment[T]): Environment[T]

    def checkCircuit(c: Circuit, env: Environment[T]): Environment[T]
  }
}
