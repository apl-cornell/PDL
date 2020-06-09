package pipedsl

import com.typesafe.scalalogging.Logger
import java.io.File
import java.nio.file.Files
import typechecker.{BaseTypeChecker, Environments, TimingTypeChecker}

object Main {
  val logger: Logger = Logger("main")

  def main(args: Array[String]): Unit = {
    logger.debug("Hello")
    val p: Parser = new Parser();
    val i: Interpreter = new Interpreter();
    if (args.length < 1) {
      throw new RuntimeException(s"Need to pass a file path as an argument")
    }
    val inputFile = new File(args(0)).toPath()
    if (!Files.exists(inputFile)) {
      throw new RuntimeException(s"File $inputFile does not exist")
    }
    val r = p.parseAll(p.prog, new String(Files.readAllBytes(inputFile)));
    logger.info(r.toString());
    val basetypes = BaseTypeChecker.check(r.get, None)
    TimingTypeChecker.check(r.get, Some(basetypes))
//    val stages = PipeCompiler.compileToDag(r.get.moddefs(0))
  //  logger.info(stages.toString())
  }

}