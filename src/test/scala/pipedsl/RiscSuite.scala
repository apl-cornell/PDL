package pipedsl

import org.scalatest.funsuite.AnyFunSuite

import java.io.File

class RiscSuite extends AnyFunSuite{
  private val folder = "src/test/tests/risc-pipe"
  private val testFiles = getListOfTests(folder)
  private val testFolder = new File(folder)

  private val inputFolder = folder + "/memInputs"
  private val inputRF = inputFolder + "/rf"
  private val inputIMEM = inputFolder + "/ti"
  private val inputDMEM = inputFolder + "/td"
  private val inputMap = Map("rf" -> inputRF, "ti" -> inputIMEM, "td" -> inputDMEM)



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
