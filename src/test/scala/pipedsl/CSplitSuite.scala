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


  test("BranchOne Parse") {
    testParse(new File(folder), new File(b1))
  }

  test("BranchOne Typecheck") {
    testTypecheck(new File(folder), new File(b1))
  }

  test("BranchTwo Parse") {
    testParse(new File(folder), new File(b2))
  }

  test("BranchTwo Typecheck") {
    testTypecheck(new File(folder), new File(b2))
  }

  test("NestedOne Parse") {
    testParse(new File(folder), new File(n1))
  }

  test("NestedOne Typecheck") {
    testTypecheck(new File(folder), new File(n1))
  }

  test("NestedTwo Parse") {
    testParse(new File(folder), new File(n2))
  }

  test("NestedTwo Typecheck") {
    testTypecheck(new File(folder), new File(n2))
  }

  test("SplitOne Parse") {
    testParse(new File(folder), new File(s1))
  }

  test("SplitOne Typecheck") {
    testTypecheck(new File(folder), new File(s1))
  }

  test("SplitTwo Parse") {
    testParse(new File(folder), new File(s2))
  }

  test("SplitTwo Typecheck") {
    testTypecheck(new File(folder), new File(s2))
  }

  test("SplitThree Parse") {
    testParse(new File(folder), new File(s3))
  }

  test("SplitThree Typecheck") {
    testTypecheck(new File(folder), new File(s3))
  }

  test("SplitFour Parse") {
    testParse(new File(folder), new File(s4))
  }

  test("SplitFour Typecheck") {
    testTypecheck(new File(folder), new File(s4))
  }
}
