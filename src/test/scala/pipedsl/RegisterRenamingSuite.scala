/* RegisterRenamingSuite.scala */
package pipedsl

import org.scalatest.funsuite.AnyFunSuite

import java.io.File

class RegisterRenamingSuite extends AnyFunSuite{
  private val folder = "src/test/tests/registerRenamingTests"
  private val testFiles = getListOfTests(folder)
  private val testFolder = new File(folder)

  private val inputFolder = folder + "/memInputs"
  private val inputRename = inputFolder + "/rename"
  private val inputM = inputFolder + "/m"
  private val inputMap = Map("rename" -> inputRename, "m" -> inputM)

  testFiles.foreach(t => {
    val testBaseName = getTestName(t)
    val simFile = getSimFile(testFolder, testBaseName)
    test((testBaseName + " Typecheck; Compile; Simulate")) {
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
