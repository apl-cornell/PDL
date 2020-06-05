package pipedsl.common

import pipedsl.common.Syntax._

//TODO better name
/*
 * This file contains syntax for the intermediate language which
 * explicitly represents pipeline stages and connections between those stages.
 * This corresponds to the language with concurrent execution semantics.
 */
object DAGSyntax {

  //Things that can send values to other things
  sealed trait Sender {
    val name: Id
  }


  class Stage(n: Id) extends Sender {
    override val name: Id = n
  }

  class BlackBox(n: Id) extends Sender {
    override val name: Id = n
  }

  class Receive(v:Id, from:Sender)
}
