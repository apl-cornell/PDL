package pipedsl.common

import DAGSyntax._
import Syntax.Id
import Utilities._

object Dataflow {

  type DFMap[T] = Map[Id, Set[T]]

  case class Analysis[T](isForward: Boolean, init: Set[T],
                         merge: (PStage, DFMap[T]) => Set[T], transfer: (PStage, Set[T]) => Set[T])

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
      val inVal: Set[T] = analysis.merge(node, inEdges(node).foldLeft[DFMap[T]](Map())((m, s) => {
        m + (s.name -> outResult(s.name))
      }))
      inResult = inResult.updated(node.name, inVal)
      val outVal: Set[T] = analysis.transfer(node, inVal)
      if (outVal != outResult(node.name)) {
        outResult = outResult.updated(node.name, outVal)
        worklist = outEdges(node).foldLeft(worklist)((wl, n) => {
          stgSet.contains(n) match {
            case true => wl :+ n
            case false => wl
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
    getUsedVars(p.cmds) ++ used -- getWrittenVars(p.cmds)
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
  def mergeUsedVars(node: PStage, used: DFMap[Id]): Set[Id] = node match {
    case stg: SpecStage => {
      //only add the variables that are used in the join
      //but not written in *either* branch
      val joinNeeds = used(stg.joinStage.name)
      val verifNeeds = used(stg.verifyStages.head.name)
      val verifWritten = joinNeeds -- joinNeeds.intersect(verifNeeds)
      val specNeeds = used(stg.specStages.head.name)
      val specWritten = joinNeeds -- joinNeeds.intersect(specNeeds)
      specNeeds.union(verifNeeds) -- specWritten -- verifWritten
    }
    case _ => used.keySet.foldLeft[Set[Id]](Set())( (s, n) => s ++ used(n))
  }

  val UsedInLaterStages = Analysis(isForward = false, Set(), mergeUsedVars, transferUsedVars)

}
