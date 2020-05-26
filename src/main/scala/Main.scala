package pipedsl
import com.typesafe.scalalogging.Logger

object Main {
  val logger: Logger = Logger("main")

  def main(args: Array[String]): Unit = {
    logger.debug("Hello")
    val p: Parser = new Parser();
    val i: Interpreter = new Interpreter();
    val r = p.parseAll(p.expr, "0xf & 7 + 11");
    val output = i.interp_expr(r.get)
    logger.info(output.toString());
  }

}