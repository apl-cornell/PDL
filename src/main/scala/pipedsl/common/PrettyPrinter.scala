package pipedsl.common

import pipedsl.common.Syntax.Command
import pprint.pprintln

object PrettyPrinter {


  def printCmd(c:Command) = {
    pprintln(c)
  }
}
