package pipedsl.typechecker

import pipedsl.common.Syntax._
import Environments.Environment

object TypeChecker {

  trait TypeChecks[U, T] {

    def emptyEnv(): Environment[U, T]

    /**
     * Given a program and an optional current envrionment mapping IDs to some type
     * information [[T]], check the program in that environment and
     * produce a new type environment
     * @param p The program to check
     * @param env The current type environment
     * @return The new type environment
     */
    def check(p: Prog, env: Option[Environment[U, T]]): Environment[U, T] = {
      val Prog(fdefs, mdefs, cir) = p
      val senv = env match { case Some(e) => e; case None => emptyEnv() }
      val fenv = fdefs.foldLeft[Environment[U, T]](senv)((tenv, fdef) => {
        checkFunc(fdef, tenv)
      })
      val menv = mdefs.foldLeft[Environment[U, T]](fenv)((tenv, mdef) => {
        checkModule(mdef, tenv)
      })
      checkCircuit(cir, menv)
    }

    def checkFunc(f: FuncDef, env:Environment[U, T]): Environment[U, T]

    def checkModule(m: ModuleDef, env: Environment[U, T]): Environment[U, T]

    def checkCircuit(c: Circuit, env: Environment[U, T]): Environment[U, T]
  }
}
