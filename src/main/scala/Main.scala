package pipedsl

import com.typesafe.scalalogging.Logger
import java.io.File
import java.nio.file.Files
import typechecker.{Environments, TypeChecker}

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
    //val output = i.interp_command(r.get)
    logger.info(r.toString());
    val fenv = TypeChecker.checkFuncDef(r.get.fdefs(0), Environments.EmptyEnv)
    val menv = TypeChecker.checkModuleDef(r.get.moddefs(0), fenv)
    //TypeChecker.checkCommand(r.get.moddefs(0).body, Environments.EmptyEnv)
  }

}