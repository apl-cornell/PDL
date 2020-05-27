package pipedsl
import com.typesafe.scalalogging.Logger

object Main {
  val logger: Logger = Logger("main")

  def main(args: Array[String]): Unit = {
    logger.debug("Hello")
    val p: Parser = new Parser();
    val i: Interpreter = new Interpreter();
    val r = p.parseAll(p.cmd, "v := 4; output v");
    val output = i.interp_command(r.get)
    logger.info(output.toString());
  }

}