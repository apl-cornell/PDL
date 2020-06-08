package pipedsl.common

import pipedsl.common.Syntax._

//TODO better name
/*
 * This file contains syntax for the intermediate language which
 * explicitly represents pipeline stages and connections between those stages.
 * This corresponds to the language with concurrent execution semantics.
 */
object DAGSyntax {

  private def channelName(from:Process, to:Process): Id = {
    Id(s"${from.name}_to_${to.name}")
  }

  case class Channel(s: Process, r: Process) {
    val name: Id = channelName(s, r)
    val sender: Process = s
    val receiver: Process = r
  }

  sealed abstract class Process(n: Id) {
    val name: Id = n
  }


  class PStage(n:Id, var preds:List[Receive], var body: List[Command], var succs: List[Send]) extends Process(n)

  class PMemory(n: Id, t: TMemType) extends Process(n) {
    val mtyp: TMemType = t
  }
  class PBlackBox(n: Id, t: TModType) extends Process(n) {
    val mtyp: TModType = t
  }

  case class Receive(g: Option[Id], into: EVar, s: Channel)
  case class Send(g: Option[Id], from: EVar, d: Channel)

  sealed trait Message
  case class MRead(src: EVar) extends Message
  case class MWrite(dest: EVar, value: EVar) extends Message
}
