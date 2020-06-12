package pipedsl.passes

import pipedsl.common.DAGSyntax.{PStage, SReceive, SSend}
import pipedsl.common.Syntax._
import pipedsl.passes.Passes.CommandPass

object SplitStagesPass extends CommandPass[PStage] {

  var stgCounter: Int = 0

  def nextStageId(): Id = {
    val s = s"Stage__$stgCounter"
    stgCounter += 1
    Id(s)
  }

  override def run(c: Command): PStage = {
    splitToStages(c).head
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
      lastc1.succs = lastc1.succs :+ (None, firstc2)
      firstc2.preds = firstc2.preds :+ (None, lastc1) //these are unconditional edges
      firstStages ++ secondStages
    }
    //TODO once we add other control structures (like split+join) update this
    case _ => {
      List(new PStage(nextStageId(), c))
    }
  }

  /**
   * Extracts all of the statements which correspond to communication
   * with another pipeline stage or memory.
   * @param c The command to modify
   * @param nextStg The next stage in the pipeline
   * @return (the modified command, the set of send statements generated, the receive statements corresponding to those sends)
   */
  private def extractReceives(c: Command, nextStg: Id): (Command, List[SSend], List[SReceive]) = c match {
    case _ => (c, List(), List())
  }
}
