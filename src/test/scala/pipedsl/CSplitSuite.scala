package pipedsl

import org.scalatest.funsuite.AnyFunSuite

import java.io.File

class CSplitSuite extends AnyFunSuite {

  private val folder = "src/test/tests/branchesCheck"
  private val b1 = folder + "/branch-1.pdl"
  private val b2 = folder + "/branch-2.pdl"
  private val n1 = folder + "/nested-1.pdl"
  private val n2 = folder + "/nested-2.pdl"
  private val s1 = folder + "/split-1.pdl"
  private val s2 = folder + "/split-2.pdl"
  private val s3 = folder + "/split-3.pdl"
  private val s4 = folder + "/split-4.pdl"


  private val testFiles = getListOfTests(folder)
  private val simFiles =  getListOfSims(folder)
  private val testFolder = new File(folder)

  testFiles.foreach(t => {
    val testBaseName = getTestName(t)
    val simFile = getSimFile(testFolder, testBaseName)

    //TODO find a way to only run the right set of tests
    //based on whether we expect success or not
    test((testBaseName + " Parse")) {
      testParse(testFolder, t)
    }

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
