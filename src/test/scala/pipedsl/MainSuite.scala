package pipedsl

import java.io.File
import java.nio.file.Paths

import org.apache.commons.io.{FileUtils, FilenameUtils}
import org.scalatest.funsuite.AnyFunSuite

import scala.reflect.io.Directory
import scala.sys.process._

class MainSuite extends AnyFunSuite{
  val pathToBluespecScript = "bin/runbsc"
  val outputFileBS = "top.sim.out"

  test("Histogram Parse Test") {
    testParse(new File("src/test/tests/histogram"),
      new File("src/test/tests/histogram/histogram.pdl"))
  }

  test("Histogram Typecheck Test") {
    testTypecheck(new File("src/test/tests/histogram"),
      new File("src/test/tests/histogram/histogram.pdl"))
  }

  test("Histogram Compilation Test") {
    testBlueSpecCompile(new File( "src/test/tests/histogram"),
      new File("src/test/tests/histogram/histogram.pdl"),
      None,
      Map("h" -> "src/test/tests/histogram/memInputs/h",
        "f" -> "src/test/tests/histogram/memInputs/f",
        "w" -> "src/test/tests/histogram/memInputs/w"))
  }
  
  test("Histogram Simulation Test") {
    testBlueSpecSim(new File( "src/test/tests/histogram"), 
      new File("src/test/tests/histogram/histogram.pdl"),
      None,
      Map("h" -> "src/test/tests/histogram/memInputs/h",
        "f" -> "src/test/tests/histogram/memInputs/f",
        "w" -> "src/test/tests/histogram/memInputs/w"))
  }

  test("Histogram BRAM Parse Test") {
    testParse(new File("src/test/tests/histogram"),
      new File("src/test/tests/histogram/histogram_bram.pdl"))
  }

  test("Histogram BRAM Typecheck Test") {
    testTypecheck(new File("src/test/tests/histogram"),
      new File("src/test/tests/histogram/histogram_bram.pdl"))
  }

  test("Histogram BRAM Compilation Test") {
    testBlueSpecCompile(new File( "src/test/tests/histogram"),
      new File("src/test/tests/histogram/histogram_bram.pdl"),
      None,
      Map("h" -> "src/test/tests/histogram/memInputs/h",
        "f" -> "src/test/tests/histogram/memInputs/f",
        "w" -> "src/test/tests/histogram/memInputs/w"))
  }

  test("Histogram BRAM Simulation Test") {
    testBlueSpecSim(new File( "src/test/tests/histogram"),
      new File("src/test/tests/histogram/histogram_bram.pdl"),
      None,
      Map("h" -> "src/test/tests/histogram/memInputs/h",
        "f" -> "src/test/tests/histogram/memInputs/f",
        "w" -> "src/test/tests/histogram/memInputs/w"))
  }

  test("Histogram SHORT Parse Test") {
    testParse(new File("src/test/tests/histogram"),
      new File("src/test/tests/histogram/histogram_short.pdl"))
  }

  test("Histogram SHORT Typecheck Test") {
    testTypecheck(new File("src/test/tests/histogram"),
      new File("src/test/tests/histogram/histogram_short.pdl"))
  }

  test("Histogram SHORT Compilation Test") {
    testBlueSpecCompile(new File( "src/test/tests/histogram"),
      new File("src/test/tests/histogram/histogram_short.pdl"),
      None,
      Map("h" -> "src/test/tests/histogram/memInputs/h",
        "f" -> "src/test/tests/histogram/memInputs/f",
        "w" -> "src/test/tests/histogram/memInputs/w"))
  }

  test("Histogram SHORT Simulation Test") {
    testBlueSpecSim(new File( "src/test/tests/histogram"),
      new File("src/test/tests/histogram/histogram_short.pdl"),
      None,
      Map("h" -> "src/test/tests/histogram/memInputs/h",
        "f" -> "src/test/tests/histogram/memInputs/f",
        "w" -> "src/test/tests/histogram/memInputs/w"))
  }
  
  test("Matrix Power Parse Test") {
    testParse(new File("src/test/tests/matpow"),
      new File("src/test/tests/matpow/matpow.pdl"))
  }
  
  test("Matrix Power Typecheck Test") {
    testTypecheck(new File("src/test/tests/matpow"),
      new File("src/test/tests/matpow/matpow.pdl"))
  }

  test("Matrix Power Compilation Test") {
    testBlueSpecCompile(new File( "src/test/tests/matpow"),
      new File("src/test/tests/matpow/matpow.pdl"),
      None,
      Map("a" -> "src/test/tests/matpow/memInputs/a_2",
        "x" -> "src/test/tests/matpow/memInputs/x",
        "r" -> "src/test/tests/matpow/memInputs/r",
        "c" -> "src/test/tests/matpow/memInputs/c"))
  }
  
  test("Matrix Power Simulation Test") {
    testBlueSpecSim(new File( "src/test/tests/matpow"),
      new File("src/test/tests/matpow/matpow.pdl"),
      None,
      Map("a" -> "src/test/tests/matpow/memInputs/a_2",
        "x" -> "src/test/tests/matpow/memInputs/x",
        "r" -> "src/test/tests/matpow/memInputs/r",
        "c" -> "src/test/tests/matpow/memInputs/c"))
  }

  test("Matrix Power BRAM Parse Test") {
    testParse(new File("src/test/tests/matpow"),
      new File("src/test/tests/matpow/matpow_bram.pdl"))
  }

  test("Matrix Power BRAM Typecheck Test") {
    testTypecheck(new File("src/test/tests/matpow"),
      new File("src/test/tests/matpow/matpow_bram.pdl"))
  }

  test("Matrix Power BRAM Compilation Test") {
    testBlueSpecCompile(new File( "src/test/tests/matpow"),
      new File("src/test/tests/matpow/matpow_bram.pdl"),
      None,
      Map("a" -> "src/test/tests/matpow/memInputs/a_2",
        "x" -> "src/test/tests/matpow/memInputs/x",
        "r" -> "src/test/tests/matpow/memInputs/r",
        "c" -> "src/test/tests/matpow/memInputs/c"))
  }

  test("Matrix Power BRAM Simulation Test") {
    testBlueSpecSim(new File( "src/test/tests/matpow"),
      new File("src/test/tests/matpow/matpow_bram.pdl"),
      None,
      Map("a" -> "src/test/tests/matpow/memInputs/a_2",
        "x" -> "src/test/tests/matpow/memInputs/x",
        "r" -> "src/test/tests/matpow/memInputs/r",
        "c" -> "src/test/tests/matpow/memInputs/c"))
  }
  
  test("Matrix Power ALT Parse Test") {
    testParse(new File("src/test/tests/matpow"),
      new File("src/test/tests/matpow/matpow_alt.pdl"))
  }

  test("Matrix Power ALT Typecheck Test") {
    testTypecheck(new File("src/test/tests/matpow"),
      new File("src/test/tests/matpow/matpow_alt.pdl"))
  }

  test("Matrix Power ALT Compilation Test") {
    testBlueSpecCompile(new File( "src/test/tests/matpow"),
      new File("src/test/tests/matpow/matpow_alt.pdl"),
      None,
      Map("a" -> "src/test/tests/matpow/memInputs/a_2",
        "x" -> "src/test/tests/matpow/memInputs/x",
        "r" -> "src/test/tests/matpow/memInputs/r",
        "c" -> "src/test/tests/matpow/memInputs/c"))
  }

  test("Matrix Power ALT Simulation Test") {
    testBlueSpecSim(new File( "src/test/tests/matpow"),
      new File("src/test/tests/matpow/matpow_alt.pdl"),
      None,
      Map("a" -> "src/test/tests/matpow/memInputs/a_2",
        "x" -> "src/test/tests/matpow/memInputs/x",
        "r" -> "src/test/tests/matpow/memInputs/r",
        "c" -> "src/test/tests/matpow/memInputs/c"))
  }
  
  test("Multiple Execution Parse Test") {
    testParse(new File("src/test/tests/multiExec"),
      new File("src/test/tests/multiExec/multiexec.pdl"))
  }  
  
  test("Multiple Execution Typecheck Test") {
    testTypecheck(new File("src/test/tests/multiExec"),
      new File("src/test/tests/multiExec/multiexec.pdl"))
  }

  test("Multiple Execution CompilationTest") {
    testBlueSpecCompile(new File( "src/test/tests/multiExec"),
      new File("src/test/tests/multiExec/multiexec.pdl"),
      None,
      Map("i" -> "src/test/tests/multiExec/memInputs/i",
        "r" -> "src/test/tests/multiExec/memInputs/r"))
  }
  
  test("Multiple Execution Simulation Test") {
    testBlueSpecSim(new File( "src/test/tests/multiExec"),
      new File("src/test/tests/multiExec/multiexec.pdl"),
      None,
      Map("i" -> "src/test/tests/multiExec/memInputs/i",
        "r" -> "src/test/tests/multiExec/memInputs/r"))
  }

  test("Multiple Execution OOO WB Parse Test") {
    testParse(new File("src/test/tests/multiExec"),
      new File("src/test/tests/multiExec/multiexec_alt.pdl"))
  }
  
  test("Multiple Execution OOO WB Typecheck Test") {
    testTypecheck(new File("src/test/tests/multiExec"),
      new File("src/test/tests/multiExec/multiexec_alt.pdl"))
  }

  test("Multiple Execution OOO WB Compilation Test") {
    testBlueSpecCompile(new File( "src/test/tests/multiExec"),
      new File("src/test/tests/multiExec/multiexec_alt.pdl"),
      None,
      Map("i" -> "src/test/tests/multiExec/memInputs/i",
        "r" -> "src/test/tests/multiExec/memInputs/r"))
  }

  test("Multiple Execution OOO WB Simulation Test") {
    testBlueSpecSim(new File( "src/test/tests/multiExec"),
      new File("src/test/tests/multiExec/multiexec_alt.pdl"),
      None,
      Map("i" -> "src/test/tests/multiExec/memInputs/i",
        "r" -> "src/test/tests/multiExec/memInputs/r"))
  }

  def testParse(testDir:File, inputFile: File): Unit = {
    Main.parse(false, true, inputFile, testDir)
    compareFiles(testDir, inputFile, "parse")
    deleteGeneratedFiles(testDir)
  }
  
  def testTypecheck(testDir:File, inputFile: File): Unit = {
    Main.runPasses(true, inputFile, testDir)
    compareFiles(testDir, inputFile, "typecheck")
    deleteGeneratedFiles(testDir)
  }

  def testBlueSpecCompile(testDir: File, inputFile: File, addrLockMod:Option[String] = None, memInit: Map[String, String]): Unit = {
    val clean = (pathToBluespecScript + " c " + testDir.getAbsolutePath).!!
    Main.gen(testDir, inputFile, false, false, addrLockMod, memInit)
    val exit = (pathToBluespecScript + " v " + testDir.getAbsolutePath).!
    assert(exit == 0)
    deleteGeneratedFiles(testDir)
    memInit.values.foreach(memPath =>
      new File(Paths.get(testDir.getAbsolutePath, FilenameUtils.getName(memPath)).toString).delete())
    new Directory(new File(Paths.get(testDir.getAbsolutePath, "Circuit_sim").toString)).deleteRecursively()
    new Directory(new File(Paths.get(testDir.getAbsolutePath, "Circuit_verilog").toString)).deleteRecursively()
    new Directory(new File(Paths.get(testDir.getAbsolutePath, "Circuit_sim").toString)).delete()
    new Directory(new File(Paths.get(testDir.getAbsolutePath, "Circuit_verilog").toString)).delete()
  }

  def testBlueSpecSim(testDir: File, inputFile: File, addrLockMod:Option[String] = None, memInit: Map[String, String]): Unit = {
    val clean = (pathToBluespecScript + " c " + testDir.getAbsolutePath).!!
    Main.gen(testDir, inputFile, false, false, addrLockMod, memInit)
    val exit = (pathToBluespecScript + " s " + testDir.getAbsolutePath).!
    assert(exit == 0)
    val blueSpecOutFile = new File(Paths.get(testDir.getAbsolutePath, outputFileBS).toString)
    val expected = new File(Paths.get(testDir.getAbsolutePath, "solutions", FilenameUtils.getBaseName(inputFile.getName) + "."+ "simsol").toString)
    println(expected)
    FileUtils.contentEquals(blueSpecOutFile , expected)
    assert(FileUtils.contentEqualsIgnoreEOL(blueSpecOutFile , expected, null))
    deleteGeneratedFiles(testDir)
    memInit.values.foreach(memPath => 
      new File(Paths.get(testDir.getAbsolutePath, FilenameUtils.getName(memPath)).toString).delete())
    new Directory(new File(Paths.get(testDir.getAbsolutePath, "Circuit_sim").toString)).deleteRecursively()
    new Directory(new File(Paths.get(testDir.getAbsolutePath, "Circuit_verilog").toString)).deleteRecursively()
    new Directory(new File(Paths.get(testDir.getAbsolutePath, "Circuit_sim").toString)).delete()
    new Directory(new File(Paths.get(testDir.getAbsolutePath, "Circuit_verilog").toString)).delete()
  }
  
  def compareFiles(testDir:File, inputFile: File, fileExtension:String): Unit = {
    val outputName = FilenameUtils.getBaseName(inputFile.getName) + "." + fileExtension
    val outputFile = new File(Paths.get(testDir.getPath, outputName).toString)
    val expected = new File(Paths.get(testDir.getPath, "solutions", outputName + "sol").toString)
    assert(FileUtils.contentEqualsIgnoreEOL(outputFile, expected, null))
  }
  
  def deleteGeneratedFiles(testDir: File): Unit = {
    testDir.listFiles.filter(f => f.getName.endsWith(".bo")||
      f.getName.endsWith(".bsv")||
      f.getName.endsWith(".ba")||
      f.getName.endsWith(".sched")||
      f.getName.endsWith(".ba")||
      f.getName.endsWith(".bexe")||
      f.getName.endsWith(".so")||
      f.getName.endsWith(".out") ||
      f.getName.endsWith(".parse")||
      f.getName.endsWith(".typecheck")||
      f.getName.endsWith(".interpret")).foreach(f => f.delete())
  }
  
}
