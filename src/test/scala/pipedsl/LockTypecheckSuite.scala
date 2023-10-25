/* LockTypecheckSuite.scala */
package pipedsl

import org.scalatest.funsuite.AnyFunSuite

import java.io.File

class LockTypecheckSuite extends AnyFunSuite {
  private val folder = "src/test/tests/typecheckTests"
  private val testFiles = getListOfTests(folder)
  private val testFolder = new File(folder)

  testFiles.foreach(t => {
    val testBaseName = getTestName(t)
    test(testBaseName + " Typecheck") {
      testTypecheck(testFolder, t)
    }

  })

}
