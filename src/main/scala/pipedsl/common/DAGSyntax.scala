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

  case class PipelineEdge(cond: Option[Expr], from: PStage, to:PStage, values: Set[Id] = Set())

  /**
   *
   * @param n
   */
  class PStage(n:Id) extends Process(n) {

    //Any outgoing communication edge, including normal pipeline flow, calls and communication with memories
    var edges: Set[PipelineEdge] = Set()

    def outEdges: Set[PipelineEdge] = {
      edges.filter(e => e.from == this)
    }
    def inEdges: Set[PipelineEdge] = {
      edges.filter(e => e.to == this)
    }

    //Set of combinational commands
    var cmds: List[Command] = List()
    //Successors for dataflow based computation. This encodes all dataflow dependencies.
    def succs: Set[PStage] = {
      edges.filter(e => e.from == this).map(e => e.to)
    }
    //Successors for dataflow based computation. This encodes all dataflow dependencies.
    def preds: Set[PStage] = {
      edges.filter(e => e.to == this).map(e => e.to)
    }
    //Used for visitor style traversals that allow types to
    //control how their children are visited
    def children: Set[PStage] = succs

    /**
     *
     * @param other
     * @param cond
     */
    def addEdgeTo(other: PStage, cond: Option[Expr] = None): Unit = {
      val edge = PipelineEdge(cond, this, other)
      addEdge(edge)
    }

    /**
     *
     * @param edge
     */
    def addEdge(edge: PipelineEdge): Unit = {
      val other = edge.to
      this.edges = this.edges + edge
      other.edges = other.edges + edge
    }

    /**
     *
     * @param other
     * @return
     */
    def removeEdgesTo(other: PStage): Set[PipelineEdge] = {
      other.edges = other.edges.filter(e => e.from != this)
      val (otherEdges, notOther) = this.edges.partition(e => e.to == other)
      this.edges = notOther
      otherEdges
    }

    /**
     *
     * @param cmd
     */
    def addCmd(cmd: Command): Unit = {
      this.cmds = this.cmds :+ cmd
    }
  }


  class SpecStage(n: Id, val specVar: EVar, val specVal: Expr,
    val verify: PStage, val lastVerify: PStage,
    val spec: PStage, val lastSpec: PStage, val joinStage: PStage) extends PStage(n) {

    override def children: Set[PStage] = Set(joinStage)
  }

  /**
   * @param n
   * @param tblock
   * @param fblock
   */
  class IfStage(n: Id, val cond: Expr, val tblock: PStage, val tend: PStage,
    val fblock: PStage, val fend: PStage, val joinStage: PStage) extends PStage(n) {
    override def children: Set[PStage] = Set(joinStage)
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
