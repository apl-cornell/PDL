/* ExceptionSuite.scala */
package pipedsl

import org.scalatest.funsuite.AnyFunSuite

import java.io.File

class ExceptionSuite extends AnyFunSuite{
  private val folder = "src/test/tests/exception"
  private val testFiles = getListOfTests(folder)
  private val testFolder = new File(folder)

  private val inputFolder = folder + "/memInputs"
  private val inputRF = inputFolder + "/rename"
  private val inputIMEM = inputFolder + "/ti"
  private val inputMap = Map("rename" -> inputRF, "ti" -> inputIMEM)

  testFiles.foreach(t => {
    val testBaseName = getTestName(t)
    val simFile = getSimFile(testFolder, testBaseName)
    test(testBaseName + " Typecheck; Compile; Simulate") {
      val doesTypecheck = testTypecheck(testFolder, t)
      if (doesTypecheck) {
        testBlueSpecCompile(testFolder, t, None, inputMap)
        if (simFile.exists) {
          testBlueSpecSim(testFolder, t, None, inputMap)
        }
      }
    }
  })
}
