package pipedsl

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
import pipedsl.typechecker.TypeInferenceWrapper.TypeInference
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
          case ("parse") => parse(debug = true, printOutput = true, config.file, config.out,
            rfLockImpl = config.defaultRegLock)
          case ("interpret") => interpret(config.maxIterations, config.memoryInput, config.file, config.out,
            rfLockImpl = config.defaultRegLock)
          case ("gen") => gen(config.out, config.file, config.printStageGraph,
            config.debug, config.memInit, config.port_warn, config.autocast,
            rfLockImpl = config.defaultRegLock, printTimer = config.printTimer)
          case ("typecheck") => runPasses(printOutput = true, config.file, config.out, config.port_warn,
            config.autocast, rfLockImpl = config.defaultRegLock)
          case _ =>
        }
      }
      case _ => 
    }
  }
  
  def parse(debug: Boolean, printOutput: Boolean, inputFile: File, outDir: File,
            rfLockImpl: Option[String] = None): Prog = {
    if (!Files.exists(inputFile.toPath)) {
      throw new RuntimeException(s"File $inputFile does not exist")
    }
    val p: Parser = new Parser(rflockImpl = rfLockImpl.getOrElse("RenameRF"))
    val prog = p.parseCode(new String(Files.readAllBytes(inputFile.toPath)))
    val outputName = FilenameUtils.getBaseName(inputFile.getName) + ".parse"
    val outputFile = new File(Paths.get(outDir.getPath, outputName).toString)
    if (printOutput) new PrettyPrinter(Some(outputFile)).printProgram(prog)
    prog
  }
  
  def interpret(maxIterations:Int, memoryInputs: Seq[String], inputFile: File, outDir: File,
                rfLockImpl: Option[String] = None): Unit = {
    val outputName = FilenameUtils.getBaseName(inputFile.getName) + ".interpret"
    val outputFile = new File(Paths.get(outDir.getPath, outputName).toString)
    val prog = parse(debug = false, printOutput = false, inputFile, outDir)
    val i: Interpreter = new Interpreter(maxIterations)
    i.interp_prog(RemoveTimingPass.run(prog), MemoryInputParser.parse(memoryInputs), outputFile)
  }
  
  def runPasses(printOutput: Boolean, inputFile: File, outDir: File, port_warn :Boolean,
      autocast: Boolean, rfLockImpl: Option[String] = None): (Prog, ProgInfo) = {
    if (!Files.exists(inputFile.toPath)) {
      throw new RuntimeException(s"File $inputFile does not exist")
    }
    val outputName = FilenameUtils.getBaseName(inputFile.getName) + ".typecheck"
    val outputFile = new File(Paths.get(outDir.getPath, outputName).toString)

    val prog = parse(debug = false, printOutput = false, inputFile, outDir, rfLockImpl = rfLockImpl)

    try {
      //val prog = ExceptingToNormal.run(ex_prog)
      // new PrettyPrinter(None).printProgram(prog)
      val pinfo = new ProgInfo(prog)
      MarkNonRecursiveModulePass.run(prog)
      //First: add lock regions + checkpoints, then do other things
      val inferredProg = new LockRegionInferencePass().run(prog)
      val verifProg = AddCheckpointHandlesPass.run(AddVerifyValuesPass.run(inferredProg))
      val canonProg2 = new CanonicalizePass().run(verifProg)
      val canonProg = new TypeInference(autocast).checkProgram(canonProg2)
      // new PrettyPrinter(None).printProgram(canonProg)

      val basetypes = BaseTypeChecker.check(canonProg, None)
      FunctionConstraintChecker.check(canonProg)
      val nprog = new BindModuleTypes(basetypes).run(canonProg)
      val recvProg = SimplifyRecvPass.run(nprog)
      LockRegionChecker.check(recvProg, None)
      val lockWellformedChecker = new LockWellformedChecker()
      val locks = lockWellformedChecker.check(canonProg)
      pinfo.addLockInfo(lockWellformedChecker.getModLockGranularityMap)
      val lockOperationTypeChecker = new LockOperationTypeChecker(lockWellformedChecker.getModLockGranularityMap)
      lockOperationTypeChecker.check(recvProg)
      val portChecker = new PortChecker(port_warn)
      portChecker.check(recvProg, None)
      val predicateGenerator = new PredicateGenerator()
      val ctx = predicateGenerator.run(recvProg)
      val lockChecker = new LockConstraintChecker(locks, lockWellformedChecker.getModLockGranularityMap, ctx)
      lockChecker.check(recvProg, None)
      LockReleaseChecker.check(recvProg)
      FinalblocksConstraintChecker.check(recvProg)
      val linChecker = new LinearExecutionChecker(ctx)
      linChecker.check(recvProg, None)
      val specChecker = new SpeculationChecker(ctx)
      specChecker.check(recvProg, None)
      val lock_prog = LockOpTranslationPass.run(recvProg)
      TimingTypeChecker.check(lock_prog, Some(basetypes))
      val exnprog = ExnTranslationPass.run(lock_prog)
      new PrettyPrinter(None).printProgram(exnprog)
      if (printOutput) {
        val writer = new PrintWriter(outputFile)
        writer.write("Passed")
        writer.close()
      }
      ctx.close()
      (exnprog, pinfo)
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
      //Add in extra conditionals to ensure address locks are not double acquired
      //TODO fix this pass -- currently no examples NEED it and thus we're OK.
      //we will need it to support more flexible versions of the lock libraries (multiple stateful modifications per cycle)
      //RemoveReentrantPass.run(stgs)
      //Must be done after all passes that introduce new variables
      AddEdgeValuePass.run(stgs)
      //remove unnecessary lock regions
      LockEliminationPass.run(stgs)
      //This pass produces a new stage list (not modifying in place)
      val newstgs = CollapseStagesPass.run(stgs)
      //we may be able to remove some more regions now that we've merged
      LockEliminationPass.run(newstgs)
      if (printStgGraph) new PrettyPrinter(None).printStageGraph(n.v, newstgs)
      n -> newstgs
    }
  }
  
  def gen(outDir: File, inputFile: File, printStgInfo: Boolean = false, debug: Boolean = false,
    memInit: Map[String, String],  portWarn: Boolean = false, autocast: Boolean, rfLockImpl: Option[String] = None,
          printTimer: Boolean = false): Unit = {
    val (prog_recv, prog_info) = runPasses(printOutput = false, inputFile, outDir, portWarn, autocast, rfLockImpl = rfLockImpl)
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

    val bsints = new BluespecInterfaces()
    val bsvgen = new BluespecProgramGenerator(prog_recv, optstageInfo, prog_info,
      debug, bsints, memInit = memInitFileNames, printTimer = printTimer)
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
