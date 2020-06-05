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

  class Channel(s: Process, r: Process) {
    val name: Id = channelName(s, r)
    val sender: Process = s
    val receiver: Process = r
  }

  sealed abstract class Process(n: Id) {
    val name: Id = n
  }

  class PStage(n:Id, var recvs:List[Receive], var body: Command, var sends: List[Send]) extends Process(n)
  class PMemory(n: Id, t: TMemType) extends Process(n) {
    val mtyp: TMemType = t
  }
  class PBlackBox(n: Id, t: TModType) extends Process(n) {
    val mtyp: TModType = t
  }

  class Receive(g: Option[Id], d: List[EVar], s: Channel) {
    val guard: Option[Id] = g
    val dest: List[EVar] = d
    val source: Channel = s
  }
  class Send(g: Option[Id], s: List[EVar], d: Channel) {
    val guard: Option[Id] = g
    val source: List[EVar] = s
    val dest: Channel = d
  }

}
