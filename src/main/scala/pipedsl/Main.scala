package pipedsl

import com.typesafe.scalalogging.Logger
import common.{BSVPrettyPrinter, MemoryInputParser, PrettyPrinter}
import java.io.File
import java.nio.file.Files

import passes.{AddEdgeValuePass, BindModuleTypes, CanonicalizePass, CollapseStagesPass, ConvertAsyncPass, LockOpTranslationPass, RemoveTimingPass, SimplifyRecvPass, SplitStagesPass}
import typechecker.{BaseTypeChecker, LockChecker, SpeculationChecker, TimingTypeChecker}
import common.Utilities._
import pipedsl.codegen.bsv.BluespecGeneration.{BluespecModuleGenerator, BluespecProgramGenerator}
import pipedsl.codegen.bsv.BluespecGeneration
import pipedsl.common.DAGSyntax.PStage
import pipedsl.common.Syntax.Id

import scala.collection.immutable
import scala.io.Source

object Main {
  val logger: Logger = Logger("main")

  def main(args: Array[String]): Unit = {
    logger.debug("Hello")
    val p: Parser = new Parser();
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
    val nprog = new BindModuleTypes(basetypes).run(prog)
    TimingTypeChecker.check(nprog, Some(basetypes))
    val prog_recv = SimplifyRecvPass.run(nprog)
    LockChecker.check(prog_recv, None)
    //TODO speculation fix in the future SpeculationChecker.check(prog_recv, Some(basetypes))
    /** How to call the interpreter
    val i: Interpreter = new Interpreter(4)
    i.interp_prog(RemoveTimingPass.run(prog_recv), MemoryInputParser.parse(args))
    **/
    
    //Done checking things
    val stageInfo: Map[Id, List[PStage]] = new SplitStagesPass().run(prog_recv)
    //Run the transformation passes on the stage representation
    val optstageInfo = stageInfo map { case (n, stgs) =>
      new ConvertAsyncPass(n).run(stgs)
      AddEdgeValuePass.run(stgs)
      LockOpTranslationPass.run(stgs)
      //This pass produces a new stage list (not modifying in place)
      val newstgs = CollapseStagesPass.run(stgs)
      PrettyPrinter.printStageGraph(n.v, newstgs)
      n -> newstgs
    }
    //Do Code Generation
    val bsvgen = new BluespecProgramGenerator(prog_recv, optstageInfo, true)
    val outputDir = "testOutputs"
    bsvgen.getBSVPrograms.foreach(p => {
      val outputFileName = p.name + ".bsv"
      val bsvWriter = BSVPrettyPrinter.getFilePrinter(name = outputDir + "/" + outputFileName);
      bsvWriter.printBSVProg(p)
    })
  }
}