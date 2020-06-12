package pipedsl.common

import pipedsl.common.Syntax._
import scala.reflect.api.Position
import scala.util.parsing.input.Positional

//TODO better name
/*
 * This file contains syntax for the intermediate language which
 * explicitly represents pipeline stages and connections between those stages.
 * This corresponds to the language with concurrent execution semantics.
 */
object DAGSyntax {

  private def channelName(from:Id, to:Id): Id = {
    Id(s"${from.v}_to_${to.v}")
  }

  case class Channel(s: Id, r: Id) {
    val name: Id = channelName(s, r)
    val sender: Id = s
    val receiver: Id = r
  }

  sealed abstract class Process(n: Id) {
    val name: Id = n
  }

  sealed trait StageCommand extends Positional

  case class SAssign(lhs: Expr, rhs: Expr) extends StageCommand
  case class SIf(cond: Expr, cons: List[StageCommand], alt: List[StageCommand]) extends StageCommand
  case class SCall(id: Id, args: List[Expr]) extends StageCommand
  case class SReceive(g: Option[Expr], into: EVar, s: Channel) extends StageCommand
  case class SSend(g: Option[Expr], from: EVar, d: Channel) extends StageCommand
  case class SOutput(exp: Expr) extends StageCommand
  case class SReturn(exp: Expr) extends StageCommand
  case class SExpr(exp: Expr) extends StageCommand
  case object SEmpty extends StageCommand

  type PipelineEdge = (Option[Expr], PStage)

  /**
   * Abstract representation of a pipeline stage
   * @param n - Unique stage identifier
   * @param cmd - The original Command that the stage executes from the input language
   * @param preds - The list (could be set?) of stages that directly precede this one in the pipeline
   * @param succs - The list (could be set?) of stages that directly follow this one in the pipeline
   * @param recvs - The set of receive statements which conditionally receive data from preds or external modules
   * @param body - The set of combinational logic that executes during this stage
   * @param sends - The set of conditional send operations to succs and/or external modules
   */
  class PStage(n:Id, var cmd: Command, var preds: List[PipelineEdge] = List(), var succs: List[PipelineEdge] = List(),
    var recvs:List[SReceive] = List(), var body: List[StageCommand] = List(), var sends: List[SSend] = List()) extends Process(n)

  class PMemory(n: Id, t: TMemType) extends Process(n) {
    val mtyp: TMemType = t
  }
  class PBlackBox(n: Id, t: TModType) extends Process(n) {
    val mtyp: TModType = t
  }

  sealed trait Message
  case class MRead(src: EVar) extends Message
  case class MWrite(dest: EVar, value: EVar) extends Message
}
