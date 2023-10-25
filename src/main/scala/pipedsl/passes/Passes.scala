/* Passes.scala */
package pipedsl.passes

import pipedsl.common.DAGSyntax.PStage
import pipedsl.common.Syntax._

object Passes {

  trait ProgPass[T] {
    def run(p: Prog): T
  }

  trait ModulePass[T] {
    def run(m: ModuleDef): T
  }

  trait FunctionPass[T] {
    def run(f: FuncDef): T
  }

  trait CommandPass[T] {
    def run(c: Command): T
  }

  trait StagePass[T] {
    def run(s: List[PStage]): T
  }

}
