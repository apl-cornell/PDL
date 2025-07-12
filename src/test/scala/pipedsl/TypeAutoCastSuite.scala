/* TypeAutoCastSuite.scala */
package pipedsl

import org.scalatest.funsuite.AnyFunSuite

import java.io.File

class TypeAutoCastSuite extends AnyFunSuite
 {
  private val folder = "src/test/tests/autocastTests"
  private val testFiles = getListOfTests(folder)
  private val testFolder = new File(folder)

  testFiles.foreach(t =>
  {val testBaseName = getTestName(t)
   test(testBaseName + " Typecheck")
   {testTypecheck(testFolder, t, autocast = true)}})
 }
