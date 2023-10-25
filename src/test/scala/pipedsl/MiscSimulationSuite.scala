/* MiscSimulationSuite.scala */
package pipedsl

import org.scalatest.funsuite.AnyFunSuite

import java.io.File

class MiscSimulationSuite extends AnyFunSuite
 {
  private val folder = "src/test/tests/simtests"
  private val testFiles = getListOfTests(folder)
  private val testFolder = new File(folder)

  testFiles.foreach(t => {
   val testBaseName = getTestName(t)
   test(testBaseName + " Typecheck") {
    testTypecheck(testFolder, t, autocast = true)
   }
   test(testBaseName + " BSV Compile") {
    testBlueSpecCompile(testFolder, t, None, Map())
   }
   test(testBaseName + " Simulate ") {
    testBlueSpecSim(testFolder, t, None, Map(), Some(testBaseName + ".simsol"))
   }
  })

 }
