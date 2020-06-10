package pipedsl.passes

import pipedsl.common.Syntax._

object Passes {

  trait CommandPass[T] {
    def run(c: Command): T
  }

}
