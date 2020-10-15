package pipedsl

import java.io.File
import java.nio.file.Files

import passes.{AddEdgeValuePass, BindModuleTypes, CanonicalizePass, CollapseStagesPass, ConvertAsyncPass, LockOpTranslationPass, MarkNonRecursiveModulePass, RemoveTimingPass, SimplifyRecvPass, SplitStagesPass}
import pipedsl.codegen.bsv.BluespecGeneration.BluespecProgramGenerator
import com.typesafe.scalalogging.Logger
import pipedsl.codegen.bsv.BSVPrettyPrinter
import pipedsl.common.DAGSyntax.PStage
import pipedsl.common.Syntax.{Id, Prog}
import pipedsl.common.{CommandLineParser, MemoryInputParser, PrettyPrinter}
import pipedsl.typechecker.{BaseTypeChecker, LockChecker, SpeculationChecker, TimingTypeChecker}

object Main {
  val logger: Logger = Logger("main")

  def main(args: Array[String]): Unit = {
    // OParser.parse returns Option[Config]
    CommandLineParser.parse(args) match {
      case Some(config) =>
        config.mode match {
          case "parse" => parse(debug = true, printOutput = true, config.out, config.file)
          case "interpret" => interpret(config.out, config.maxIterations, config.memoryInput, config.file)
          case "gen" => gen(config.out, config.file, config.printStageGraph, config.debug)
        }
      case _ =>
    }
  }

  def parse(debug: Boolean, printOutput: Boolean, outputFile: File, inputFile: File): Prog = {
    if (!Files.exists(inputFile.toPath)) {
      throw new RuntimeException(s"File $inputFile does not exist")
    }
    val p: Parser = new Parser()
    val r = p.parseAll(p.prog, new String(Files.readAllBytes(inputFile.toPath)))
    if (printOutput) new PrettyPrinter(Some(outputFile)).printProgram(r.get)
    r.get

  }

  def interpret(outputFile: File, maxIterations:Int, memoryInputs: Seq[String], inputFile: File): Unit = {
    val prog = parse(debug = false, printOutput = false, outputFile, inputFile)
    val i: Interpreter = new Interpreter(maxIterations)
    i.interp_prog(RemoveTimingPass.run(prog), MemoryInputParser.parse(memoryInputs), outputFile.toString)
  }

  //TODO can refactor even further and add options for individual passes
  def runPasses(prog: Prog): Prog = {
    val canonProg = CanonicalizePass.run(prog)
    val basetypes = BaseTypeChecker.check(canonProg, None)
    val nprog = new BindModuleTypes(basetypes).run(canonProg)
    TimingTypeChecker.check(nprog, Some(basetypes))
    MarkNonRecursiveModulePass.run(nprog)
    val recvProg = SimplifyRecvPass.run(nprog)
    LockChecker.check(recvProg, None)
    SpeculationChecker.check(recvProg, Some(basetypes))
    recvProg
  }

  def getStageInfo(prog: Prog, printStgGraph: Boolean): Map[Id, List[PStage]] = {
    //Done checking things
    val stageInfo: Map[Id, List[PStage]] = new SplitStagesPass().run(prog)
    //Run the transformation passes on the stage representation
    stageInfo map { case (n, stgs) =>
      new ConvertAsyncPass(n).run(stgs)
      LockOpTranslationPass.run(stgs)
      //Must be done after all passes that introduce new variables
      AddEdgeValuePass.run(stgs)
      //This pass produces a new stage list (not modifying in place)
      val newstgs = CollapseStagesPass.run(stgs)
      if (printStgGraph) new PrettyPrinter(None).printStageGraph(n.v, newstgs)
      n -> newstgs
    }
  }

  def gen(outDir: File, inputFile: File, printStgInfo: Boolean = false, debug: Boolean = false): Unit = {
    val prog = parse(debug = false, printOutput = false, outDir, inputFile)
    val prog_recv = runPasses(prog)
    val optstageInfo = getStageInfo(prog_recv, printStgInfo)
    val bsvgen = new BluespecProgramGenerator(prog_recv, optstageInfo, debug)
    //ensure directory exists
    outDir.mkdirs()
    bsvgen.getBSVPrograms.foreach(p => {
      val outFile = new File(outDir.toString + "/" + p.name + ".bsv")
      val bsvWriter = BSVPrettyPrinter.getFilePrinter(name = outFile)
      bsvWriter.printBSVProg(p)
    })
  }
}