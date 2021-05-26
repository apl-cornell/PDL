package pipedsl

import com.microsoft.z3.Context

import java.io.{File, PrintWriter}
import java.nio.file.{Files, Paths, StandardCopyOption}
import com.typesafe.scalalogging.Logger
import org.apache.commons.io.FilenameUtils
import pipedsl.codegen.bsv.{BSVPrettyPrinter, BluespecInterfaces}
import pipedsl.codegen.bsv.BluespecGeneration.BluespecProgramGenerator
import pipedsl.common.DAGSyntax.PStage
import pipedsl.common.Syntax.{Id, Prog}
import pipedsl.common.{CommandLineParser, MemoryInputParser, PrettyPrinter, ProgInfo}
import pipedsl.passes._
import pipedsl.typechecker._

object Main {
  val logger: Logger = Logger("main")

  def main(args: Array[String]): Unit = {
    // OParser.parse returns Option[Config]
    CommandLineParser.parse(args) match {
      case Some(config) => {
        //In case directories don't exist
        config.out.mkdirs()
        (config.mode) match {
          case ("parse") => parse(debug = true, printOutput = true, config.file, config.out)
          case ("interpret") => interpret(config.maxIterations, config.memoryInput, config.file, config.out)
          case ("gen") => gen(config.out, config.file, config.printStageGraph,
            config.debug, config.defaultAddrLock, config.memInit, config.addSubInts)
          case ("typecheck") => runPasses(printOutput = true, config.file, config.out)
          case _ =>
        }
      }
      case _ => 
    }
  }
  
  def parse(debug: Boolean, printOutput: Boolean, inputFile: File, outDir: File): Prog = {
    if (!Files.exists(inputFile.toPath)) {
      throw new RuntimeException(s"File $inputFile does not exist")
    }
    val p: Parser = new Parser()
    val r = p.parseAll(p.prog, new String(Files.readAllBytes(inputFile.toPath)))
    val outputName = FilenameUtils.getBaseName(inputFile.getName) + ".parse"
    val outputFile = new File(Paths.get(outDir.getPath, outputName).toString)
    if (printOutput) new PrettyPrinter(Some(outputFile)).printProgram(r.get)
    r.get
  }
  
  def interpret(maxIterations:Int, memoryInputs: Seq[String], inputFile: File, outDir: File): Unit = {
    val outputName = FilenameUtils.getBaseName(inputFile.getName) + ".interpret"
    val outputFile = new File(Paths.get(outDir.getPath, outputName).toString)
    val prog = parse(debug = false, printOutput = false, inputFile, outDir)
    val i: Interpreter = new Interpreter(maxIterations)
    i.interp_prog(RemoveTimingPass.run(prog), MemoryInputParser.parse(memoryInputs), outputFile)
  }
  
  def runPasses(printOutput: Boolean, inputFile: File, outDir: File): (Prog, ProgInfo) = {
    if (!Files.exists(inputFile.toPath)) {
      throw new RuntimeException(s"File $inputFile does not exist")
    }
    val outputName = FilenameUtils.getBaseName(inputFile.getName) + ".typecheck"
    val outputFile = new File(Paths.get(outDir.getPath, outputName).toString)

    val prog = parse(debug = false, printOutput = false, inputFile, outDir)
    val pinfo = new ProgInfo(prog)
    try {
      val verifProg = AddVerifyValuesPass.run(prog)
      val canonProg = CanonicalizePass.run(verifProg)
      val basetypes = BaseTypeChecker.check(canonProg, None)
      val nprog = new BindModuleTypes(basetypes).run(canonProg)
      TimingTypeChecker.check(nprog, Some(basetypes))
      MarkNonRecursiveModulePass.run(nprog)
      val recvProg = SimplifyRecvPass.run(nprog)
      LockRegionChecker.check(recvProg, None)
      val lockWellformedChecker = new LockWellformedChecker()
      val locks = lockWellformedChecker.check(canonProg)
      pinfo.addLockInfo(lockWellformedChecker.getModLockGranularityMap)
      val lockOperationTypeChecker = new LockOperationTypeChecker(lockWellformedChecker.getModLockGranularityMap)
      lockOperationTypeChecker.check(recvProg)
      val predicateGenerator = new PredicateGenerator()
      val ctx = predicateGenerator.run(recvProg)
      val lockChecker = new LockConstraintChecker(locks, lockWellformedChecker.getModLockGranularityMap, ctx)
      lockChecker.check(recvProg, None)
      LockReleaseChecker.check(recvProg)
      SpeculationChecker.check(recvProg, Some(basetypes))
      if (printOutput) {
        val writer = new PrintWriter(outputFile)
        writer.write("Passed")
        writer.close()
      }
      (recvProg, pinfo)
    } catch {
      case t: Throwable => {
        //If fails, print the error to the file
        if (printOutput) {
          val writer = new PrintWriter(outputFile)
          writer.write("Failed")
          writer.close()
        }
        throw t
      }
    } 
  }

  def getStageInfo(prog: Prog, printStgGraph: Boolean): Map[Id, List[PStage]] = {
    //Done checking things
    val stageInfo: Map[Id, List[PStage]] = new SplitStagesPass().run(prog)
    //Run the transformation passes on the stage representation
    stageInfo map { case (n, stgs) =>
      //Change Recv statements into send + recv pairs
      new ConvertAsyncPass(n).run(stgs)
      //Convert lock ops into ops that track explicit handles
      LockOpTranslationPass.run(stgs)
      //Add in extra conditionals to ensure address locks are not double acquired
      RemoveReentrantPass.run(stgs)
      //Must be done after all passes that introduce new variables
      AddEdgeValuePass.run(stgs)
      //This pass produces a new stage list (not modifying in place)
      val newstgs = CollapseStagesPass.run(stgs)
      //clean up lock ops that need to be merged at this point
      LockEliminationPass.run(newstgs)
      if (printStgGraph) new PrettyPrinter(None).printStageGraph(n.v, newstgs)
      n -> newstgs
    }
  }
  
  def gen(outDir: File, inputFile: File, printStgInfo: Boolean = false, debug: Boolean = false,
    addrLockMod: Option[String] = None, memInit: Map[String, String], addSubInts: Boolean = false): Unit = {
    val (prog_recv, prog_info) = runPasses(printOutput = false, inputFile, outDir)
    val optstageInfo = getStageInfo(prog_recv, printStgInfo)
    //TODO better way to pass configurations to the BSInterfaces object
    //copies mem initialization to output directory
    for(fileName <- memInit.values) {
      val targetPath = Paths.get(outDir.getAbsolutePath, new File(fileName).getName)
      Files.copy(Paths.get(fileName), targetPath, StandardCopyOption.REPLACE_EXISTING)
    }
    //Need to transform map passes into the program generator
    val memInitFileNames = memInit.foldLeft[Map[String,String]](Map())((map, kv) =>
      map + (kv._1 -> new File(kv._2).getName))

    val bsints = new BluespecInterfaces(addrLockMod)
    val bsvgen = new BluespecProgramGenerator(prog_recv, optstageInfo, prog_info,
      debug, bsints, memInit = memInitFileNames, addSubInts = addSubInts)
    val funcWriter = BSVPrettyPrinter.getFilePrinter(new File(outDir.toString + "/" + bsvgen.funcModule + ".bsv"))
    funcWriter.printBSVFuncModule(bsvgen.getBSVFunctions)
    funcWriter.close
    bsvgen.getBSVPrograms.foreach(p => {
      val outFile = new File(outDir.toString + "/" + p.name + ".bsv")
      val bsvWriter = BSVPrettyPrinter.getFilePrinter(name = outFile)
      bsvWriter.printBSVProg(p)
      bsvWriter.close
    })
  }
}
