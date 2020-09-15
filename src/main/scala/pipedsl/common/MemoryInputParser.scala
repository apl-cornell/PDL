package pipedsl.common

import scala.collection.immutable
import scala.io.Source

/**
 * Parser to parse command line arguments for input test memories. Currently, memory file arguments are in the 
 * format "($memoryName, $filePath, $memorySize)"
 */
object MemoryInputParser {
  
  def parse(args: Array[String]): Map[String, Array[Int]] = {
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
    memories
  }

}
