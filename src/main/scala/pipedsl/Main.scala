package pipedsl

import java.nio.file.{Files, Path}

import com.typesafe.scalalogging.Logger
import pipedsl.codegen.BluespecGeneration.BluespecProgramGenerator
import pipedsl.common.DAGSyntax.PStage
import pipedsl.common.Syntax.{Id, Prog}
import pipedsl.common.{BSVPrettyPrinter, CommandLineParser, MemoryInputParser, PrettyPrinter}
import pipedsl.passes._
import pipedsl.typechecker.{BaseTypeChecker, LockChecker, SpeculationChecker, TimingTypeChecker}

object Main {
  val logger: Logger = Logger("main")

  def main(args: Array[String]): Unit = {
    // OParser.parse returns Option[Config]
    CommandLineParser.parse(args) match {
      case Some(config) =>
        config.mode match {
          case "parse" => parse(true, config.file.toPath)
          case "interpret" => interpret(config.out.toPath, config.maxIterations, config.memoryInput, config.file.toPath)
          case "gen" => gen(config.out.toPath, config.file.toPath)
        }
      case _ => 
    }
  }
  
  //TODO Add printing to file for parse
  def parse(debug: Boolean, inputFile: Path) = {
    if (!Files.exists(inputFile)) {
      throw new RuntimeException(s"File $inputFile does not exist")
    }
    val p: Parser = new Parser()
    val r = p.parseAll(p.prog, new String(Files.readAllBytes(inputFile)))
    r.get
  }
  
  def interpret(outputFile: Path, maxIterations:Int, memoryInputs: Seq[String], inputFile: Path): Unit = {
    val prog = parse(false, inputFile)
    val i: Interpreter = new Interpreter(maxIterations)
    i.interp_prog(RemoveTimingPass.run(prog), MemoryInputParser.parse(memoryInputs), outputFile.toString)
  }
  
  //TODO can refactor even further and add options for individual passes
  def runPasses(prog: Prog): Prog = {
    val prog1 = CanonicalizePass.run(prog)
    val basetypes = BaseTypeChecker.check(prog1, None)
    TimingTypeChecker.check(prog1, Some(basetypes))
    val prog_recv = SimplifyRecvPass.run(prog1)
    LockChecker.check(prog_recv, None)
    SpeculationChecker.check(prog_recv, Some(basetypes))
    prog_recv
  }
  
  def getStageInfo(prog_recv: Prog): Map[Id, List[PStage]] = {
    //Done checking things
    val stageInfo: Map[Id, List[PStage]] = new SplitStagesPass().run(prog_recv)
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
  }
  
  def gen(outputFile: Path, inputFile: Path): Unit = {
    val prog = parse(false, inputFile)
    val prog_recv = runPasses(prog)
    val optstageInfo = getStageInfo(prog_recv)
    val bsvgen = new BluespecProgramGenerator(prog_recv, optstageInfo)
    bsvgen.getBSVPrograms.foreach(p => {
      val bsvWriter = BSVPrettyPrinter.getFilePrinter(name = outputFile.toString);
      bsvWriter.printBSVProg(p)
    })
  }
  
}