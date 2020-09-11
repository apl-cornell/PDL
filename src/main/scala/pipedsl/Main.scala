package pipedsl

import com.typesafe.scalalogging.Logger
import common.BSVPrettyPrinter
import java.io.File
import java.nio.file.Files

import passes.{AddEdgeValuePass, CanonicalizePass, ConvertRecvPass, LockOpTranslationPass, SimplifyRecvPass, SplitStagesPass}
import typechecker.{BaseTypeChecker, LockChecker, SpeculationChecker, TimingTypeChecker}
import common.Utilities._
import pipedsl.codegen.BluespecGeneration
import pipedsl.codegen.BluespecGeneration.BluespecModuleGenerator

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
    val stageInfo = new SplitStagesPass().run(prog_recv)
    //Run the transformation passes on the stage representation
    stageInfo.foreachEntry( (n, s) => {
      val mod = prog_recv.moddefs.find(m => m.name == n).getOrThrow(new RuntimeException())
      new ConvertRecvPass().run(s)
      AddEdgeValuePass.run(s)
      LockOpTranslationPass.run(s)
      //Code Generation
      val outputDir = "testOutputs";
      val outputFileName = mod.name.v.capitalize + ".bsv"
      val bsvWriter = BSVPrettyPrinter.getFilePrinter(name = outputDir + "/" + outputFileName);
      val bsvgenerator = new BluespecModuleGenerator(mod, s.head, flattenStageList(s.tail))
      bsvWriter.printBSVProg(bsvgenerator.getBSV)
    })
    //TODO accept output type and location options
  }

}