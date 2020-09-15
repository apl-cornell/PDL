package pipedsl

import com.typesafe.scalalogging.Logger
import common.{BSVPrettyPrinter, MemoryInputParser}
import java.io.File
import java.nio.file.Files

import passes.{AddEdgeValuePass, CanonicalizePass, ConvertRecvPass, RemoveTimingPass, SimplifyRecvPass, SplitStagesPass}
import typechecker.{BaseTypeChecker, LockChecker, SpeculationChecker, TimingTypeChecker}
import common.Utilities._
import pipedsl.codegen.BluespecGeneration

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
    val r = p.parseAll(p.prog, new String(Files.readAllBytes(inputFile)));
    val prog = CanonicalizePass.run(r.get)
    //logger.info(prog.toString)
    val basetypes = BaseTypeChecker.check(prog, None)
    TimingTypeChecker.check(prog, Some(basetypes))
    val prog_recv = SimplifyRecvPass.run(prog)
    LockChecker.check(prog_recv, None)
    SpeculationChecker.check(prog_recv, Some(basetypes))
    val stageInfo = SplitStagesPass.run(prog_recv)
    val i: Interpreter = new Interpreter(4)
    i.interp_prog(RemoveTimingPass.run(prog_recv), MemoryInputParser.parse(args))
    stageInfo.foreachEntry( (n, s) => {
      val mod = prog_recv.moddefs.find(m => m.name == n).getOrThrow(new RuntimeException())
      val convertrecv = new ConvertRecvPass()
      convertrecv.run(s)
      AddEdgeValuePass.run(s)
      val bsvWriter = BSVPrettyPrinter.getFilePrinter("testOutputs/one.bsv")
      val bsvprog = BluespecGeneration.getBSV(mod, s.head, flattenStageList(s.tail))
      bsvWriter.printBSVProg(bsvprog)
      //BluespecGeneration.run(s.head, mod.inputs, s.tail)
      //PrettyPrinter.printStages(s)
      //PrettyPrinter.printStageGraph(n.v, s)

    })
  }

}