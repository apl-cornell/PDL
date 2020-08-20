package pipedsl

import com.typesafe.scalalogging.Logger
import common.PrettyPrinter
import java.io.File
import java.nio.file.Files

import passes.{AddEdgeValuePass, BluespecGeneration, CanonicalizePass, SimplifyRecvPass, SplitStagesPass}
import typechecker.{BaseTypeChecker, LockChecker, SpeculationChecker, TimingTypeChecker}
import common.Utilities._

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
    val prog = CanonicalizePass.run(r.get)
    //logger.info(prog.toString)
    val basetypes = BaseTypeChecker.check(prog, None)
    TimingTypeChecker.check(prog, Some(basetypes))
    val prog_recv = SimplifyRecvPass.run(prog)
    LockChecker.check(prog_recv, None)
    SpeculationChecker.check(prog_recv, Some(basetypes))
    val stageInfo = SplitStagesPass.run(prog_recv)
    stageInfo.foreachEntry( (n, s) => {
      val mod = prog_recv.moddefs.find(m => m.name == n).getOrThrow(new RuntimeException())
      AddEdgeValuePass.run(s)
      val bsvprog = BluespecGeneration.getBSV(s.head, mod.inputs.map(p => p.name), flattenStageList(s.tail))
      pprint.pprintln(bsvprog.modules)
      //BluespecGeneration.run(s.head, mod.inputs, s.tail)
      //PrettyPrinter.printStages(s)
      //PrettyPrinter.printStageGraph(n.v, s)

    })
  }

}