package pipedsl.passes

import pipedsl.common.DAGSyntax.{IfStage, PStage, PipelineEdge, SReceive, SSend, SpecStage}
import pipedsl.common.Syntax._
import pipedsl.common.Utilities._
import Passes.{CommandPass, ModulePass, ProgPass}
import pipedsl.common.{Dataflow, Syntax}


object SplitStagesPass extends CommandPass[List[PStage]] with ModulePass[List[PStage]] with ProgPass[Map[Id, List[PStage]]] {

  var stgCounter: Int = 0

  def nextStageId(): Id = {
    val s = s"Stage__$stgCounter"
    stgCounter += 1
    Id(s)
  }


  override def run(p: Prog): Map[Id,List[PStage]] = {
    p.moddefs.foldLeft(Map[Id,List[PStage]]())((m, mod) => {
      m + (mod.name -> run(mod))
    })
  }

  override def run(m: ModuleDef): List[PStage] = run(m.body)

  override def run(c: Command): List[PStage] = {
    val startStage = new PStage(Id("Start"))
    val stages = splitToStages(c, startStage)
    stages
  }

  /**
   *
   * @param c
   * @return
   */
  private def splitToStages(c: Command, curStage: PStage): List[PStage] = c match {
    case CTBar(c1, c2) => {
      val leftStages = splitToStages(c1, curStage)
      val nextStage = new PStage(nextStageId())
      val rightStages = splitToStages(c2, nextStage)
      //sequential pipeline, end of left sends to beginning of right
      leftStages.last.addEdgeTo(nextStage)
      leftStages ++ rightStages
    }
    case CSpeculate(predVar, predVal, verify, body) => {
      val firstVerif = new PStage(nextStageId())
      val verifStages = splitToStages(verify, firstVerif)
      val firstSpec = new PStage(nextStageId())
      val specStages = splitToStages(body, firstSpec)
      val joinStage = new PStage(nextStageId())
      val specStage = new SpecStage(nextStageId(), predVar, predVal, verifStages, specStages, joinStage)
      curStage.addEdgeTo(specStage)
      List(curStage, specStage, joinStage)
    }
    case CIf(cond, cons, alt) => {
      val firstTrueStage = new PStage(nextStageId())
      val trueStages = splitToStages(cons, firstTrueStage)
      val firstFalseStage = new PStage(nextStageId())
      val falseStages = splitToStages(alt, firstFalseStage)
      val joinStage = new PStage(nextStageId())
      val ifStage = new IfStage(nextStageId(), cond, trueStages, falseStages, joinStage)
      curStage.addEdgeTo(ifStage)
      List(curStage, ifStage, joinStage)
    }
    case CSeq(c1, c2) => {
      val nstage = splitToStages(c1, curStage)
      nstage.init ++ splitToStages(c2, nstage.last)
    }
    case CEmpty => List(curStage)
    case _ => curStage.addCmd(c); List(curStage)
  }



  /**
   * If p may execute a 'call' command,
   * add an edge from p to the first stage in the pipeline
   * @param p - Stage to check for call statements
   * @param firstStage - First pipeline stage
   */
  private def addCallSuccs(p: PStage, firstStage: PStage): Unit = {
    p.cmds.foreach(c => callSuccsHelper(c, p, firstStage))
  }

  private def callSuccsHelper(c: Command, p:PStage, firstStage: PStage): Unit = c match {
    case CSeq(c1, c2) => callSuccsHelper(c1, p, firstStage); callSuccsHelper(c2, p, firstStage)
    case CIf(_, cons, alt) => callSuccsHelper(cons, p, firstStage); callSuccsHelper(alt, p, firstStage)
    case CCall(_, _) => {
      p.addEdgeTo(firstStage)
    }
    case CSpeculate(_, _, verify, body) => callSuccsHelper(verify, p, firstStage); callSuccsHelper(body, p, firstStage)
    case ICondCommand(_, c) => callSuccsHelper(c, p, firstStage);
    case _ => ()
  }

  /**
   *
   * @param cond
   * @param stg
   */
  private def addConditionalExecution(cond: Expr, stg: PStage): Unit = {
    val oldCmds = stg.cmds
    val newCmds = oldCmds.foldLeft(List[Command]())((l,c)  => c match {
      case CEmpty => l
      case _ => l :+ ICondCommand(cond, c)
    })
    stg.cmds = newCmds
  }

  /**
   *
   * @param left
   * @param right
   */
  private def mergeStages(left: PStage, right: PStage): Unit = {
    left.cmds = left.cmds ++ right.cmds
    val succs = right.succs
    val preds = right.preds
    succs.foreach(s => {
      val removed = right.removeEdgesTo(s)
      removed.foreach(r => left.addEdge(r))
    })
    preds.foreach(s => {
      val removed = s.removeEdgesTo(right)
      removed.map(e => PipelineEdge(e.condSend, e.condRecv, left, right)).foreach(r => s.addEdge(r))
    })
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
