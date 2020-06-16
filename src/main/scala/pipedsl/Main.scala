package pipedsl

import com.typesafe.scalalogging.Logger
import common.PrettyPrinter
import common.Dataflow._
import java.io.File
import java.nio.file.Files
import passes.{SimplifyRecvPass, SplitStagesPass}
import typechecker.{BaseTypeChecker, LockChecker, SpeculationChecker, TimingTypeChecker}

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
    val r = p.parseAll(p.prog, new String(Files.readAllBytes(inputFile)));
    val prog = r.get
    logger.info(r.toString)
    val basetypes = BaseTypeChecker.check(prog, None)
    TimingTypeChecker.check(prog, Some(basetypes))
    val prog_recv = SimplifyRecvPass.run(prog)
    LockChecker.check(prog_recv, None)
    SpeculationChecker.check(prog_recv, Some(basetypes))
    val stages = SplitStagesPass.run(prog_recv.moddefs.head.body)
    val (df_ins, df_outs) = worklist(stages, UsedInLaterStages)
    PrettyPrinter.printCmd(prog_recv.moddefs.head.body)
    stages.foreach(s => {
      logger.info("Stage: " + s.name)
      logger.info("Variables Needed After This Stages: ")
      logger.info(df_outs(s.name).mkString(","))
    })

    //val stages = PipeCompiler.compileToDag(r.get.moddefs(0))
    //logger.info(stages.toString())
  }

}