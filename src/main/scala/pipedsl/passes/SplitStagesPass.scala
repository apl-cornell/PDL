package pipedsl.passes

import pipedsl.common.DAGSyntax.{PStage, SReceive, SSend}
import pipedsl.common.Syntax
import pipedsl.common.Syntax._
import pipedsl.passes.Passes.CommandPass

object SplitStagesPass extends CommandPass[List[PStage]] {

  var stgCounter: Int = 0

  def nextStageId(): Id = {
    val s = s"Stage__$stgCounter"
    stgCounter += 1
    Id(s)
  }

  override def run(c: Command): List[PStage] = {
    val termStage = new PStage(Id("Terminate"), CEmpty)
    var stageList: List[PStage] = splitToStages(c)
    //Add the terminator stage
    stageList = stageList.map[PStage](p => {
      if (p.succs.isEmpty) {
        p.succs = p.succs :+ termStage
        termStage.preds = termStage.preds :+ p
      }
      p
    })
    stageList = stageList :+ termStage
    //Add the backedges to each stage that sends data to the beginning of the pipeline
    stageList.foreach(p => addCallSuccs(p, stageList.head))
    stageList
  }

  /**
   * If p may execute a 'call' command,
   * add an edge from p to the first stage in the pipeline
   * @param p - Stage to check for call statements
   * @param firstStage - First pipeline stage
   */
  private def addCallSuccs(p: PStage, firstStage: PStage): Unit = {
    callSuccsHelper(p.cmd, p, firstStage)
  }

  private def callSuccsHelper(c: Command, p:PStage, firstStage: PStage): Unit = c match {
    case CSeq(c1, c2) => callSuccsHelper(c1, p, firstStage); callSuccsHelper(c2, p, firstStage)
    case CIf(_, cons, alt) => callSuccsHelper(cons, p, firstStage); callSuccsHelper(alt, p, firstStage)
    case CCall(_, _) => {
      p.succs = p.succs :+ firstStage
      firstStage.preds = firstStage.preds :+ p
    }
    case CSpeculate(_, _, body) => callSuccsHelper(body, p, firstStage)
    case _ => ()
  }
  /**
   *
   * @param c
   * @return
   */
  private def splitToStages(c: Command): List[PStage] = c match {
    case CTBar(c1, c2) => {
      val firstStages = splitToStages(c1)
      val secondStages = splitToStages(c2)
      //sequential pipeline, last stage in c1 sends to first in c2
      val lastc1 = firstStages.last
      val firstc2 = secondStages.head
      lastc1.succs = lastc1.succs :+ firstc2
      firstc2.preds = firstc2.preds :+ lastc1
      firstStages ++ secondStages
    }
    //TODO once we add other control structures (like split+join) update this
    case _ => {
      List(new PStage(nextStageId(), c))
    }
  }

  /**
   * Extracts all of the statements which correspond to communication
   * with memory or external modules.
   * @param c The command to modify
   * @param nextStg The next stage in the pipeline
   * @return (the modified command, the set of send statements generated, the receive statements corresponding to those sends)
   */
  private def extractReceives(c: Command, nextStg: Id): (Command, List[SSend], List[SReceive]) = c match {
    case _ => (c, List(), List())
  }
}
