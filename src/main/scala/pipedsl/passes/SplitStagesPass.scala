package pipedsl.passes

import pipedsl.common.DAGSyntax.{IfStage, PStage, PipelineEdge}
import pipedsl.common.Syntax._
import Passes.{CommandPass, ModulePass, ProgPass}

/**
 * This pass converts the original imperative program specifications into
 * graphs of staged execution. These stages are defined by the TBar separators.
 * The most interesting points are control points (like IF statements) where
 * extra PSTages are added to enforce this conditional execution structure.
 * However these are split into a unique type of PStage so that later passes
 * can elide some of the extra stages that were added (see the CollapseStagesPass)
 */
class SplitStagesPass extends CommandPass[List[PStage]] with ModulePass[List[PStage]] with ProgPass[Map[Id, List[PStage]]] {

  private var stgCounter: Int = 0

  private def nextStageId(): Id = {
    val s = s"Stage__$stgCounter"
    stgCounter += 1
    Id(s)
  }

  override def run(p: Prog): Map[Id,List[PStage]] = {
    p.moddefs.foldLeft(Map[Id,List[PStage]]())((m, mod) => {
      m + (mod.name -> run(mod))
    })
  }

  override def run(m: ModuleDef): List[PStage] = {
    stgCounter = 0
    val stgs = run(m.body)
    val emptyStage = new PStage(Id("_input_"))
    //Add an edge to a not-real stage to simplify
    //communication compilation later
    val inputEdge = PipelineEdge(None, None, emptyStage, stgs.head, m.inputs.map(i => i.name).toSet)
    stgs.head.addEdge(inputEdge)
    stgs
  }

  override def run(c: Command): List[PStage] = {
    val startStage = new PStage(Id("Start"))
    val stages = splitToStages(c, startStage)
    stages
  }

  private def splitToStages(c: Command, curStage: PStage): List[PStage] = c match {
    case CTBar(c1, c2) =>
      val leftStages = splitToStages(c1, curStage)
      val nextStage = new PStage(nextStageId())
      val rightStages = splitToStages(c2, nextStage)
      //sequential pipeline, end of left sends to beginning of right
      leftStages.last.addEdgeTo(nextStage)
      leftStages ++ rightStages
    case CIf(cond, cons, alt) =>
      val firstTrueStage = new PStage(nextStageId())
      val trueStages = splitToStages(cons, firstTrueStage)
      val firstFalseStage = new PStage(nextStageId())
      val falseStages = splitToStages(alt, firstFalseStage)
      val joinStage = new PStage(nextStageId())
      val ifStage = new IfStage(nextStageId(), List(cond), List(trueStages), falseStages, joinStage)
      curStage.addEdgeTo(ifStage)
      List(curStage, ifStage, joinStage)
    case CSplit(cases, default) =>
      val condStages = cases.map(c => splitToStages(c.body, new PStage(nextStageId())))
      val defaultStage = splitToStages(default, new PStage(nextStageId()))
      val joinStage = new PStage(nextStageId())
      val ifStage = new IfStage(nextStageId(), cases.map(c => c.cond), condStages, defaultStage, joinStage)
      curStage.addEdgeTo(ifStage)
      List(curStage, ifStage, joinStage)
    case CSeq(c1, c2) =>
      val nstage = splitToStages(c1, curStage)
      nstage.init ++ splitToStages(c2, nstage.last)
    case CEmpty() => List(curStage)
    case _ => curStage.addCmd(c); List(curStage)
  }

}
