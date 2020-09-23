package pipedsl.common

import java.io.File

import scopt.OParser

object CommandLineParser {
  
  case class Config(
                     out: File = new File("."),
                     debug: Boolean = false,
                     mode: String = "",
                     file: File = new File("."),
                     memoryInput: Seq[String] = Seq(),
                     maxIterations: Int = 0
                   )

  def buildParser(): OParser[Unit, Config] = {
    val builder = OParser.builder[Config]
    val parser1 = {
      import builder._
      OParser.sequence(
        programName("pipedsl"),
        head("pipedsl", "0.0.1"),
        opt[File]('o', "out")
          .required()
          .valueName("<file>")
          .action((x, c) => c.copy(out = x))
          .text("out is the file to print the output to"),
        help('h', "help").text("prints this usage text"),
        arg[File]("<file>...")
          .required()
          .action((x, c) => c.copy(file = x))
          .text("pdsl files to parse"),
        cmd("parse")
          .text("parses the provided pdsl file and prints to the out file\n")
          .action((_, c) => c.copy(mode = "parse")),
        cmd("interpret")
          .action((_, c) => c.copy(mode = "interpret"))
          .text("interprets the provided pdsl file and prints the resulting memory state to the out file\n")
          .children(
            opt[Int]("maxIterations")
              .action((x,c) => c.copy(maxIterations = x))
              .required()
              .text("iterations to run the interpreter for"),
            opt[Seq[String]]("memoryInput")
              .valueName("<memory1>,<memory2>...")
              .action((x, c) => c.copy(memoryInput = x))
              .required()
              .unbounded()
              .text("provided memory inputs")
          ),
        cmd("gen")
          .text("generates code for the provided pdsl file and prints the generated bluespec to the out file\n")
          .action((_, c) => c.copy(mode = "gen"))
      )
    }
    parser1
  }
  
  def parse(args: Array[String]): Option[Config] = {
    OParser.parse(buildParser(), args, Config())
  }
}
