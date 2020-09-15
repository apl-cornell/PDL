package pipedsl

import com.typesafe.scalalogging.Logger
import common.{BSVPrettyPrinter, PrettyPrinter}
import java.io.File
import java.nio.file.Files

import passes.{AddEdgeValuePass, CanonicalizePass, CollapseStagesPass, ConvertRecvPass, LockOpTranslationPass, SimplifyRecvPass, SplitStagesPass}
import typechecker.{BaseTypeChecker, LockChecker, SpeculationChecker, TimingTypeChecker}
import common.Utilities._
import pipedsl.codegen.BluespecGeneration
import pipedsl.codegen.BluespecGeneration.{BluespecModuleGenerator, BluespecProgramGenerator}
import pipedsl.common.DAGSyntax.PStage
import pipedsl.common.Syntax.Id

object Main {
  val logger: Logger = Logger("main")

  def main(args: Array[String]): Unit = {
    logger.debug("Hello")
    val p: Parser = new Parser();
    val i: Interpreter = new Interpreter();
    if (args.length < 1) {
      throw new RuntimeException(s"Need to pass a file path as an argument")
    }
    val inputFile = new File(args(0)).toPath
    if (!Files.exists(inputFile)) {
      throw new RuntimeException(s"File $inputFile does not exist")
    }
    //TODO option to parse different syntax forms (mostly for testing)
    val r = p.parseAll(p.prog, new String(Files.readAllBytes(inputFile)));
    //TODO pull all of this into a different class/function that organizes IR transformation passes and code generation
    val prog = CanonicalizePass.run(r.get)
    val basetypes = BaseTypeChecker.check(prog, None)
    TimingTypeChecker.check(prog, Some(basetypes))
    val prog_recv = SimplifyRecvPass.run(prog)
    LockChecker.check(prog_recv, None)
    SpeculationChecker.check(prog_recv, Some(basetypes))
    //Done checking things
    val stageInfo: Map[Id, List[PStage]] = SplitStagesPass.run(prog_recv)
    //Run the transformation passes on the stage representation
    stageInfo map { case (n, stgs) =>
      new ConvertRecvPass().run(stgs)
      AddEdgeValuePass.run(stgs)
      LockOpTranslationPass.run(stgs)
      //This pass produces a new stage list (not modifying in place)
      val newstgs = CollapseStagesPass.run(stgs)
      PrettyPrinter.printStageGraph(n.v, newstgs)
      n -> newstgs
    }
    //Do Code Generation
    val bsvgen = new BluespecProgramGenerator(prog_recv, stageInfo)
    val outputDir = "testOutputs"
    bsvgen.getBSVPrograms.foreach(p => {
      val outputFileName = p.name + ".bsv"
      val bsvWriter = BSVPrettyPrinter.getFilePrinter(name = outputDir + "/" + outputFileName);
      bsvWriter.printBSVProg(p)
    })
  }

}