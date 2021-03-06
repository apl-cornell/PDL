package pipedsl.common

import DAGSyntax._
import Syntax.{Id, LockArg}
import Utilities._
import pipedsl.common.Locks._

object Dataflow {

  type DFMap[T] = Map[Id, T]

  case class Analysis[T](isForward: Boolean, init: T,
                         merge: (PStage, DFMap[T]) => T, transfer: (PStage, T) => T)

  /**
   * This is the worklist algorithm for dataflow analysis, which only operates on sets of
   * values at the moment.
   * A key features is that it only follows edges to stages listed in the input.
   * If a "pred" or "succ" isn't in *stages* then it won't follow that edge.
   * @param stages - The stages that make up the pipeline (similar to basic blocks in normal DF)
   * @param analysis - The object containing all of the analysis functions and parameters.
   * @tparam T - The type of information we're tracking in the DF sets.
   * @return A tuple whose left element contains the map of "IN" values
   *         for each stage and whose right contains the map of "OUT" values for each stage.
   */
  def worklist[T](stages: List[PStage], analysis: Analysis[T]): (DFMap[T], DFMap[T]) = {
    var firstBlock = stages.head
    val stgSet: Set[PStage] = stages.toSet
    var inEdges: PStage => Set[PStage] = (p: PStage) => p.preds
    var outEdges: PStage => Set[PStage] = (p: PStage) => p.succs
    //Flip for backwards analysis
    if (!analysis.isForward) {
      firstBlock = stages.last
      inEdges = (p: PStage) => p.succs
      outEdges = (p: PStage) => p.preds
    }
    //Init
    var inResult: DFMap[T] = Map(firstBlock.name -> analysis.init)
    var outResult: DFMap[T] = Map().withDefaultValue(analysis.init)

    //Iterate
    var worklist = stages
    while (worklist.nonEmpty) {
      val node: PStage = worklist.head
      worklist = worklist.tail
      val inVal: T = analysis.merge(node, inEdges(node).foldLeft[DFMap[T]](Map())((m, s) => {
        m + (s.name -> outResult(s.name))
      }))
      inResult = inResult.updated(node.name, inVal)
      val outVal: T = analysis.transfer(node, inVal)
      if (outVal != outResult(node.name)) {
        outResult = outResult.updated(node.name, outVal)
        worklist = outEdges(node).foldLeft(worklist)((wl, n) => {
          if (stgSet.contains(n)) {
            wl :+ n
          } else {
            wl
          }
        })
      }
    }
    //Result
    if (analysis.isForward) {
      (inResult, outResult)
    } else {
      (outResult, inResult)
    }
  }

  /**
   * Transfer function that accumulates all variables which will
   * be used in later stages. Meant for a backwards analysis where
   * "used" contains variables which will definitely be used in
   * later stages. This adds any variables which were used in this
   * stage but not written in this stage.
   * @param p The stage to check
   * @param used The set of variables used later in the pipeline,
   * @return The set of variables used in *p* and later in the pipeline.
   */
   def transferUsedVars(p: PStage, used: Set[Id]): Set[Id] = {
    getUsedVars(p.getCmds) ++ used -- getWrittenVars(p.getCmds)
    //if a var was written this stage, then we don't need to send it from the prior stage
  }

  /**
   * Merge function that accumulates all variables used
   * in all later stages together.
   * @param node The node for whom these are incoming edges.
   *             Used primarily for varying behavior based on type.
   * @param used Variables used in later stages.
   * @return All variables unioned together
   */
  def mergeUsedVars(node: PStage, used: DFMap[Set[Id]]): Set[Id] = {
    used.keySet.foldLeft[Set[Id]](Set())( (s, n) => s ++ used(n))
  }


  /**
   * This takes in the sets of variables that can be sent by earlier
   * stages and simply unions them together (since they all are available now).
   * @param node
   * @param canSend
   * @return
   */
  def mergeCanSend(node: PStage, canSend: DFMap[Set[Id]]): Set[Id] = {
    canSend.keySet.foldLeft[Set[Id]](Set())( (s, n) => s ++ canSend(n))
  }

  def transferCanSend(p: PStage, earlierCan: Set[Id]): Set[Id] = {
    earlierCan ++ getWrittenVars(p.getCmds)
    //if a var was written this stage, or a prior stage, then we can send it to the next stage
  }

  val UsedInLaterStages: Analysis[Set[Id]] =
    Analysis[Set[Id]](isForward = false, Set(), mergeUsedVars, transferUsedVars)
  def CanSendToLaterStages(inputs: Set[Id]): Analysis[Set[Id]] =
    Analysis[Set[Id]](isForward = true, inputs, mergeCanSend, transferCanSend)
  val LockStateInfo: Analysis[Map[LockArg, LockState]] = {
    Analysis[Map[LockArg, LockState]](isForward = true, Map(), Locks.mergeLockStates, Locks.transferLockStates)
  }
  val MaybeReservedHandles: Analysis[Map[Id, Set[LockHandleInfo]]] =
    Analysis[Map[Id, Set[LockHandleInfo]]](isForward = true, Map(),
      Locks.mergeMaybeReserved, Locks.transferMaybeReserved)
}
