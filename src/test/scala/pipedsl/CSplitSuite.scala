package pipedsl

import org.scalatest.funsuite.AnyFunSuite

import java.io.File

class CSplitSuite extends AnyFunSuite {

  private val folder = "src/test/tests/branchesCheck"
  private val testFiles = getListOfTests(folder)
  private val simFiles =  getListOfSims(folder)
  private val testFolder = new File(folder)
  //TODO better way to set this up (right now convention
  //requires test only has memories containing 32-bit values i and r -> they may omit these and it will still work

  private val memInputs = folder + "/memInputs"
  def defaultInputMap(testName: String): Map[String,String] = Map(
    "ti" -> (memInputs + "/i_" + testName ),
    "tr" -> (memInputs + "/r_" + testName )
  )

  testFiles.foreach(t => {
    val testBaseName = getTestName(t)
    val simFile = getSimFile(testFolder, testBaseName)

    //TODO find a way to only run the right set of tests
    //based on whether we expect success or not

    /* for now, skip these as they aren't that useful atm.
    test((testBaseName + " Parse")) {
      testParse(testFolder, t)
    }*/

    var doesTypecheck = false
    test((testBaseName + " Typecheck")) {
      doesTypecheck = testTypecheck(testFolder, t)
    }

    if (doesTypecheck) {
      test((testBaseName + " BSV Compile")) {
        testBlueSpecCompile(testFolder, t, None, Map())
      }
    }

    if (doesTypecheck && simFile.exists) {
      test((testBaseName + " Simulation")) {
        testBlueSpecSim(testFolder, t, None, defaultInputMap(testBaseName))
      }
    }
  })

}
