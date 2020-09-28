package pipedsl.test

import java.io.{File, IOException}
import java.nio.file.Paths
import java.util.Scanner

import org.apache.commons.io.{FileUtils, FilenameUtils}

/**
 * Testing class
 */
object TestingMain {

  /**
   * Tests a function in Main by comparing the output file with the expected file.
   * 
   * @param function function to test. The inputs to this function should be an input file and an output directory
   * @param mode the type of function. This can be parse, interpret, typecheck, or gen
   * @param testInputDir the directory of the test files
   * @param testResultDir the directory of the solution files
   */
  def test(function: (File, File) => Any, mode: String, testInputDir: File, testResultDir: File): Unit = {
    if (testInputDir.exists && testInputDir.isDirectory && 
        testResultDir.exists && testResultDir.isDirectory) {
      val files = testInputDir.listFiles().filter(_.isFile).toList
      for (file <- files) {
        val outputName = FilenameUtils.getBaseName(file.getName) + "." + mode
        val outputFile = new File(Paths.get(testResultDir.getPath, outputName).toString)
        val expected = new File(Paths.get(testResultDir.getPath, outputName + "sol").toString)
        try {
          function(file, testResultDir)
          println(file + ": " + (if (FileUtils.contentEquals(outputFile, expected)) "Passed" else "Failed"))
        } catch  {
            case io: IOException => throw io
            case _: Throwable => {
              println(file + ": " + (if (new Scanner(expected).next() == "Failed") "Passed" else "Failed"))
            }
        }
      }
    } else {
      throw new RuntimeException("The testing directories does not exist")
    }
  }
}
