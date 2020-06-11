package pipedsl.passes

import pipedsl.common.Syntax._

object Passes {

  trait ProgPass[T] {
    def run(p: Prog): T
  }

  trait ModulePass[T] {
    def run(m: ModuleDef): T
  }

  trait CommandPass[T] {
    def run(c: Command): T
  }

}
