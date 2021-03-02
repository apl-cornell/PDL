package pipedsl

import org.scalatest.funsuite.AnyFunSuite

import java.io.File

class CSplitSuite extends AnyFunSuite {

  private val folder = "src/test/tests/branchesCheck"
  private val testFiles = getListOfTests(folder)
  private val simFiles =  getListOfSims(folder)
  private val testFolder = new File(folder)

  testFiles.foreach(t => {
    val testBaseName = getTestName(t)
    val simFile = getSimFile(testFolder, testBaseName)

    //TODO find a way to only run the right set of tests
    //based on whether we expect success or not

    /* for now, skip these as they aren't that useful atm.
    test((testBaseName + " Parse")) {
      testParse(testFolder, t)
    }*/

    test((testBaseName + " Typecheck")) {
      testTypecheck(testFolder, t)
    }


    test((testBaseName + " BSV Compile")) {
      testBlueSpecCompile(testFolder, t, None, Map())
    }

    if (simFile.exists) {
      test((testBaseName + " Simulation")) {
        testBlueSpecSim(testFolder, t, None, Map())
      }
    }
  })

}
