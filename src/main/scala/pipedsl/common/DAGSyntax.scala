package pipedsl.common

import pipedsl.common.Errors.UnexpectedCommand
import pipedsl.common.Syntax._
import pipedsl.common.Utilities.{log2, updateListMap}

/**
 * This file contains syntax for the intermediate language which
 * explicitly represents pipeline stages and connections between those stages.
 * This corresponds to the language with concurrent execution semantics.
 */
object DAGSyntax {

  case class PipelineEdge(condSend: Option[Expr], condRecv: Option[Expr],
    from: PStage, to:PStage, values: Set[Id] = Set())

  def addValues(vals: Set[Id], e: PipelineEdge): PipelineEdge = {
    PipelineEdge(e.condSend, e.condRecv, e.from, e.to, e.values ++ vals)
  }

  sealed abstract class Process(n: Id) {
    val name: Id = n
  }

  /**
   * The representation of a processing stage. Each stage
   * will execute in a single cycle and is connected to other
   * stages via edgges that are realized as Registers or FIFOs.
   * @param n The identifier for this stage; these should be unique.
   */
  class PStage(n:Id) extends Process(n) {

    //Any outgoing communication edge, including normal pipeline flow, calls and communication with memories
    private var edges: Set[PipelineEdge] = Set()

    def allEdges: Set[PipelineEdge] = edges

    def outEdges: Set[PipelineEdge] = {
      edges.filter(e => e.from == this)
    }
    def inEdges: Set[PipelineEdge] = {
      edges.filter(e => e.to == this)
    }

    //Set of combinational commands
    private var cmds: List[Command] = List()

    def getCmds: List[Command] = cmds

    /**
     *  The successors for dataflow based computation. This encodes all dataflow dependencies.
     * @return A set of stages that immediately succeede this one.
     */
    def succs: Set[PStage] = {
      edges.filter(e => e.from == this).map(e => e.to)
    }
    /**
     * The predecessors for dataflow based computation. This encodes all dataflow dependencies.
     * @return A set of stages that immediately precede this one.
     */
    def preds: Set[PStage] = {
      edges.filter(e => e.to == this).map(e => e.from)
    }

    /**
     * This modifies this stage and other in place, adding a communication edge
     * from this one to other. We need both conditional sending and receiving,
     * since sometimes data is sent conditionally but always read if it ever appears,
     * or vice versa.
     * @param other The stage to add an edge to.
     * @param condSend Some(c) if this stage only conditionally sends data to other
     * @param condRecv Some(c) if other only conditionally receives data from this
     */
    def addEdgeTo(other: PStage, condSend: Option[Expr] = None, condRecv: Option[Expr] = None,
        vals: Set[Id] = Set()): Unit = {
      val edge = PipelineEdge(condSend, condRecv, this, other, vals)
      addEdge(edge)
    }

    /**
     * This modifies this stage in place by addinge edge to the edge set.
     * Whichever stage is on the other end of edge also has edge added to its edge set.
     * Use this only if edge actually refers to this in either its from or to fields.
     * @param edge The new edge to add.
     */
    def addEdge(edge: PipelineEdge): Unit = {
      val other = if (edge.to == this) { edge.from } else { edge.to }
      this.edges = this.edges + edge
      other.edges = other.edges + edge
    }

    /**
     * Overwrite the existing edge set for this stage. Every edge
     * in edges should have this as either its from or to field.
     * This correctly deletes existing edges and adds the new edges
     * so that the other stages in the graph are updated consistently.
     * @param edges The new set of edges to replace the old set.
     */
    def setEdges(edges: Set[PipelineEdge]): Unit = {
      val toRemove = this.edges
      toRemove.foreach(e => this.removeEdge(e))
      edges.foreach(e => this.addEdge(e))
    }

    /**
     * Remove a given edge from the stage graph.
     * This updates both this stage and the connected stage.
     * This stage must be referenced by the to or from fields of edge.
     * @param edge The edge to remove.
     */
    def removeEdge(edge: PipelineEdge): Unit = {
      val other = if (edge.to == this) { edge.from } else { edge.to }
      this.edges = this.edges - edge
      other.edges = other.edges - edge
    }

    /**
     * Removes all edges from this to other.
     * This consistently updates both this stage and other to reflect the changes.
     * @param other The stage to disconnect from this one
     * @return The set of edges that were removed.
     */
    def removeEdgesTo(other: PStage): Set[PipelineEdge] = {
      other.edges = other.edges.filter(e => e.from != this)
      val (otherEdges, notOther) = this.edges.partition(e => e.to == other)
      this.edges = notOther
      otherEdges
    }

    /**
     * Adds a command to the end of this stage's command list.
     * @param cmd The Command to add
     */
    def addCmd(cmd: Command): Unit = {
      this.cmds = this.cmds :+ cmd
    }

    /**
     * Adds a list of commands to the end of this stage's command list.
     * @param cmds The Commands to add
     */
    def addCmds(cmds: Iterable[Command]): Unit = {
      this.cmds = this.cmds ++ cmds
    }

    /**
     * Overwrite the existing commands with the new list.
     * @param cmds The new commands to overwrite with.
     */
    def setCmds(cmds: Iterable[Command]): Unit = {
      this.cmds = cmds.toList
    }

    //returns lock cmds on left and non lock commands on right
    //lock commands are stored as a map from the relevant lock ID to the set of commands
    private def splitLockCmds(cmds: Iterable[Command]): (Map[LockArg, List[Command]], List[Command]) = {
      var lockCmds = Map[LockArg, List[Command]]()
      var nonlockCmds = List[Command]()
      cmds.foreach {
        case ICondCommand(cond , cs) =>
          val (lcs, nlcs) = cs.partition(c => isLockStmt(c))
          if (lcs.nonEmpty) {
            var tempMap = Map[LockArg, List[Command]]()
            lcs.foreach(lc => {
              val lockiden = getLockId(lc)
              tempMap = updateListMap(tempMap, lockiden, lc)
            })
            tempMap.keySet.foreach(k => {
              lockCmds = updateListMap(lockCmds, k, ICondCommand(cond, tempMap(k)))
            })
          }
          if (nlcs.nonEmpty) {
            nonlockCmds = nonlockCmds :+ ICondCommand(cond, nlcs)
          }
        case c if isLockStmt(c) =>
          val lockiden = getLockId(c)
          lockCmds = updateListMap(lockCmds, lockiden, c)
        case c => nonlockCmds = nonlockCmds :+ c
      }
      (lockCmds, nonlockCmds)
    }

    /**
     * Add the new commands either to the end or beginning of this stage.
     */
    def mergeStmts(newCmds: Iterable[Command], addAfter: Boolean = true): Unit = {
      val finalCmds = if (addAfter) {
        this.getCmds ++ newCmds
      } else {
        newCmds ++ this.getCmds
      }
      this.setCmds(finalCmds)
    }
  }


  private def isLockStmt(c: Command): Boolean = c match {
    case _:ICheckLockOwned |
         _:IReserveLock | _:IReleaseLock | _:ILockNoOp => true
    case _ => false
  }

  def getLockIds(cs: Iterable[Command]): List[LockArg] = {
    cs.foldLeft(List[LockArg]())((l, c) => {
      if (isLockStmt(c)) {
        l :+ getLockId(c)
      } else {
        l
      }
    })
  }

  private def getLockId(c: Command): LockArg = c match {
    case ICheckLockOwned(l, _, _) => l
    case IReserveLock(_, l) => l
    case IReleaseLock(l, _) => l
    case ILockNoOp(l) => l
    case _ => throw UnexpectedCommand(c)
  }


  /**
   * This subclass is used to represent conditional execution as a set of stages,
   * coordinated by this stage. This stage will generate the conditional expression
   * and send that to the join stage to indicate which of the branches were followed.
   * Additionally, it will send data to the head of either the true or false stages based
   * on the result of the condition
   * @param n The unique identifier for this stage
   * @param cond The conditional expression
   * @param trueStages The list of stages to execute if cond is true
   * @param falseStages The list of stages to execute if cond is false
   * @param joinStage The stage to execute after one of the branches.
   */
  class IfStage(n: Id, val conds: List[Expr], var condStages: List[List[PStage]],
    var defaultStages: List[PStage], val joinStage: PStage) extends PStage(n) {

    override def succs: Set[PStage] = {
      super.succs + joinStage
    }
    val defaultNum = conds.size
    val condVar = EVar(Id("__cond" + n.v))
    val intSize = log2(defaultNum)
    condVar.typ = Some(TSizedInt(TBitWidthLen(intSize), TUnsigned()))
    condVar.id.typ = condVar.typ
    var eTernary = ETernary(conds(defaultNum - 1), EInt(defaultNum - 1, bits = intSize), EInt(defaultNum, bits = intSize))
    for(i <- defaultNum-2 to 0 by -1 ) {
      eTernary = ETernary(conds(i), EInt(i, bits = intSize), eTernary.copy())
    }
    this.addCmd(CAssign(condVar, eTernary))
    for (i <- 0 until defaultNum) {
      this.addEdgeTo(condStages(i).head, condSend = Some (EBinop(EqOp("=="), condVar, EInt(i, bits = intSize))))
      condStages(i).last.addEdgeTo(joinStage, condRecv = Some (EBinop(EqOp("=="), condVar, EInt(i, bits = intSize))))
    }
    this.addEdgeTo(defaultStages.head, condSend = Some( EBinop(EqOp("=="), condVar, EInt(defaultNum, bits = intSize))))
    defaultStages.last.addEdgeTo(joinStage, condRecv = Some( EBinop(EqOp("=="), condVar, EInt(defaultNum, bits = intSize))))
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
