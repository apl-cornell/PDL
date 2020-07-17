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
  case class STermStage(g: Option[Expr], vals: List[EVar], dest: List[PStage]) extends StageCommand
  case class SOutput(exp: Expr) extends StageCommand
  case class SReturn(exp: Expr) extends StageCommand
  case class SExpr(exp: Expr) extends StageCommand
  case object SEmpty extends StageCommand

  case class PipelineEdge(cond: Option[Expr], to:PStage, values: Set[Id] = Set())

  /**
   *
   * @param n
   */
  class PStage(n:Id) extends Process(n) {

    //Any outgoing communication edge, including normal pipeline flow, calls and communication with memories
    var outEdges: Set[PipelineEdge] = Set()
    //Ditto, but incoming
    var inEdges: Set[PipelineEdge] = Set()
    //Set of combinational commands
    var cmds: List[Command] = List()
    //Pipeline stages which may succeed this one
    var succs: Set[PStage] = Set()
    //Pipeline stages which may precede this one
    var preds: Set[PStage] = inEdges.map(e => e.to)

    /**
     *
     * @param other
     * @param cond
     */
    def addEdgeTo(other: PStage, cond: Option[Expr] = None): Unit = {
      val edge = PipelineEdge(cond, other)
      addEdge(edge)
    }

    /**
     *
     * @param edge
     */
    def addEdge(edge: PipelineEdge): Unit = {
      val other = edge.to
      this.outEdges = this.outEdges + edge
      this.succs = this.succs + other
      other.preds = other.preds + this
    }

    /**
     *
     * @param other
     * @return
     */
    def removeEdgesTo(other: PStage): Set[PipelineEdge] = {
      val (otherEdges, notOther) = this.outEdges.partition(e => e.to == other)
      this.outEdges = notOther
      this.succs = this.succs - other
      other.preds = other.preds - this
      otherEdges
    }
    def addCmd(cmd: Command): Unit = {
      this.cmds = this.cmds :+ cmd
    }
  }


  class SpecStage(n: Id, val specVar: EVar, val specVal: Expr,
    val verfiy: PStage, val lastVerify: PStage, val spec: PStage, val lastSpec: PStage) extends PStage(n) {
    //Commands added to the SpecStage are meant to occur *after* the speculation resolution point

    /*
     //For now throw this part in current stage, maybe can move it to speculative side
      curStage.addCmd(CAssign(predVar, predVal).setPos(c.pos))
      val specId = Id("__spec_" + predVar.id)
      //lock this speculative region and start speculation
      curStage.addCmd(CLockOp(specId, LockState.Acquired))
      curStage.addCmd(ISpeculate(specId, predVar).setPos(c.pos))

       //TODO need to check that this set of commands actually assigns to predVar
      val originalSpecVar = EVar(specId)
      originalSpecVar.typ = predVar.typ
      lastVerif.addCmd(IUpdate(specId, predVar, originalSpecVar).setPos(c.pos))

       lastSpec.addCmd(CCheck(specId))
      //Need an edge to resend data on failure
      lastVerif.addEdgeTo(firstSpec, Some(EBinop(EqOp("=="), originalSpecVar, predVar)));

        //release speculative lock once both branches arrive
      joinStage.addCmd(CLockOp(specId, LockState.Released))
     */
  }

  /**
   * @param n
   * @param tblock
   * @param fblock
   */
  class IfStage(n: Id, val cond: Expr, val tblock: PStage, val tend: PStage, val fblock: PStage, val fend: PStage) extends PStage(n) {
  //Commands added to the Ifstage are meant to occur *after* the end of the if block join
  }

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
