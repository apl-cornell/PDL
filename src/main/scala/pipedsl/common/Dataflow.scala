package pipedsl.common

import DAGSyntax._
import Syntax.Id
import Utilities._

object Dataflow {

  type DFMap[T] = Map[Id, Set[T]]

  case class Analysis[T](isForward: Boolean, init: Set[T],
                         merge: DFMap[T] => Set[T], transfer: (PStage, Set[T]) => Set[T])

  def worklist[T](firstStage: PStage, analysis: Analysis[T]): (DFMap[T], DFMap[T]) = {
    val stages = getReachableStages(firstStage)
    worklist(stages, analysis)
  }

  /**
   * This is the worklist algorithm for dataflow analysis, which only operates on sets of
   * values at the moment.
   * @param stages - The stages that make up the pipeline (similar to basic blocks in normal DF)
   * @param analysis - The object containing all of the analysis functions and parameters.
   * @tparam T - The type of information we're tracking in the DF sets.
   * @return A tuple whose left element contains the map of "IN" values
   *         for each stage and whose right contains the map of "OUT" values for each stage.
   */
  def worklist[T](stages: Set[PStage], analysis: Analysis[T]): (DFMap[T], DFMap[T]) = {
    var firstBlock = stages.head
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
    var outResult: DFMap[T] = stages.foldLeft[DFMap[T]](Map())((m, s) => {
      m + (s.name -> analysis.init)
    })
    //Iterate
    var worklist = stages
    while (worklist.nonEmpty) {
      val node: PStage = worklist.head
      worklist = worklist.tail
      val inVal: Set[T] = analysis.merge(inEdges(node).foldLeft[DFMap[T]](Map())((m, s) => {
        m + (s.name -> outResult(s.name))
      }))
      inResult = inResult.updated(node.name, inVal)
      val outVal: Set[T] = analysis.transfer(node, inVal)
      if (outVal != outResult(node.name)) {
        outResult = outResult.updated(node.name, outVal)
        worklist = worklist ++ outEdges(node)
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
    getUsedVars(p.cmds) ++ used -- getWrittenVars(p.cmds)
    //if a var was written this stage, then we don't need to send it from the prior stage
  }

  /**
   * Merge function that accumulates all variables used
   * in all later stages together.
   * @param used Variables used in later stages.
   * @return All variables unioned together
   */
  def mergeUsedVars(used: DFMap[Id]): Set[Id] = {
    used.keySet.foldLeft[Set[Id]](Set())( (s, n) => s ++ used(n))
  }

  val UsedInLaterStages = Analysis(false, Set(), mergeUsedVars, transferUsedVars)

}
