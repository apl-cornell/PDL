package pipedsl

import com.typesafe.scalalogging.Logger
import common.BSVPrettyPrinter
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
    
    //Users can define test memories by adding "(<memoryname>, <text file path for memory contents>, <memory address size>)"
    if (args.length > 1) {
      val memCli=  "\\(([a-zA-Z_][a-zA-Z0-9_]*),(.*),(\\d *)\\)".r
      var memories = immutable.HashMap[String, Array[Int]]()
      for (index <- 1 until args.length) {
        args(index) match {
          case memCli(iden, file, address) => {
            val inputFile = Source.fromFile(file).getLines.toArray.map(s => Integer.decode(s).toInt)
            val memSize = Math.pow(2, Integer.parseInt(address)).toInt
            if (inputFile.length > memSize) {
              throw new RuntimeException(s"Size of memory is not big enough for provided memory")
            }
            val memArray = Array.copyOf(inputFile, memSize)
            memories = memories + (iden -> memArray)
          }
          case _ => throw new RuntimeException(s"Malformed command line arguments")
        }
      }
      //change the max iterations depending on how many instructions you feed in
      val i: Interpreter = new Interpreter(4);
      i.interp_prog(RemoveTimingPass.run(prog_recv), memories)
    }

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