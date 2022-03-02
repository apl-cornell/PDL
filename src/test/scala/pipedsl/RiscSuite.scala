package pipedsl

import org.scalatest.funsuite.AnyFunSuite

import java.io.File

class RiscSuite extends AnyFunSuite {
  private val folder = "src/test/tests/risc-pipe"
  private val testFiles = getListOfTests(folder)
  private val testFolder = new File(folder)

  private val inputFolder = folder + "/memInputs"
  private val inputRF = inputFolder + "/rf"
  private val inputIMEM = inputFolder + "/ti"
  private val inputDMEM = inputFolder + "/td"
  private val inputCmem = inputFolder + "/cmem"
  private val mainMem = inputFolder + "/mm"

  private def getInputMap(s: String): Map[String, String] = {
    Map("rf" -> inputRF, "ti" -> (inputIMEM + s), "td" -> (inputDMEM + s), "cmem" -> inputCmem,
      "mm" -> (mainMem +s) )
  }

  private val sims = getListOfSims(folder).map(s => getTestName(s))

  //For each processor impl
  testFiles.foreach(t => {
    val testBaseName = getTestName(t)
    test(testBaseName + " Typecheck") {
      testTypecheck(testFolder, t)
    }
    test(testBaseName + " BSV Compile") {
      testBlueSpecCompile(testFolder, t, None, Map())
    }
    //For each program
    sims.foreach(s => {
      val simInputs = getInputMap(s)
      test(testBaseName + " Simulate " + s) {
        testBlueSpecSim(testFolder, t, None, simInputs, Some(s + ".simsol"))
      }
    })
  })
}
