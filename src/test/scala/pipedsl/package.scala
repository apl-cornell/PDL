import java.io.File
import java.nio.file.Paths

import org.apache.commons.io.{FileUtils, FilenameUtils}

import scala.reflect.io.Directory
import scala.sys.process._

package object pipedsl {
  val pathToBluespecScript = "bin/runbsc"
  val outputFileBS = "top.sim.out"


def testParse(testDir: File, inputFile: File): Unit = {
    Main.parse(debug = false, printOutput = true, inputFile, testDir)
    compareFiles(testDir, inputFile, "parse")
    deleteGeneratedFiles(testDir)
  }

  def testTypecheck(testDir: File, inputFile: File): Unit = {
    try {
      Main.runPasses(printOutput = true, inputFile, testDir)
    } catch {
      case _: Throwable => ()
    }
    compareFiles(testDir, inputFile, "typecheck")
    deleteGeneratedFiles(testDir)
  }

  def testBlueSpecCompile(testDir: File, inputFile: File, addrLockMod: Option[String] = None, memInit: Map[String, String]): Unit = {
    val _ = (pathToBluespecScript + " c " + testDir.getAbsolutePath).!!
    Main.gen(testDir, inputFile, printStgInfo = false, debug = false, addrLockMod, memInit)
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

  def testBlueSpecSim(testDir: File, inputFile: File, addrLockMod: Option[String] = None, memInit: Map[String, String]): Unit = {
    val _ = (pathToBluespecScript + " c " + testDir.getAbsolutePath).!!
    Main.gen(testDir, inputFile, printStgInfo = false, debug = false, addrLockMod, memInit)
    val exit = (pathToBluespecScript + " s " + testDir.getAbsolutePath).!
    assert(exit == 0)
    val blueSpecOutFile = new File(Paths.get(testDir.getAbsolutePath, outputFileBS).toString)
    val expected = new File(Paths.get(testDir.getAbsolutePath, "solutions", FilenameUtils.getBaseName(inputFile.getName) + "." + "simsol").toString)
    println(expected)
    FileUtils.contentEquals(blueSpecOutFile, expected)
    assert(FileUtils.contentEqualsIgnoreEOL(blueSpecOutFile, expected, null))
    deleteGeneratedFiles(testDir)
    memInit.values.foreach(memPath =>
      new File(Paths.get(testDir.getAbsolutePath, FilenameUtils.getName(memPath)).toString).delete())
    new Directory(new File(Paths.get(testDir.getAbsolutePath, "Circuit_sim").toString)).deleteRecursively()
    new Directory(new File(Paths.get(testDir.getAbsolutePath, "Circuit_verilog").toString)).deleteRecursively()
    new Directory(new File(Paths.get(testDir.getAbsolutePath, "Circuit_sim").toString)).delete()
    new Directory(new File(Paths.get(testDir.getAbsolutePath, "Circuit_verilog").toString)).delete()
  }

  def compareFiles(testDir: File, inputFile: File, fileExtension: String): Unit = {
    val outputName = FilenameUtils.getBaseName(inputFile.getName) + "." + fileExtension
    val outputFile = new File(Paths.get(testDir.getPath, outputName).toString)
    val expected = new File(Paths.get(testDir.getPath, "solutions", outputName + "sol").toString)
    assert(FileUtils.contentEqualsIgnoreEOL(outputFile, expected, null))
  }

  def deleteGeneratedFiles(testDir: File): Unit = {
    testDir.listFiles.filter(f => f.getName.endsWith(".bo") ||
      f.getName.endsWith(".bsv") ||
      f.getName.endsWith(".ba") ||
      f.getName.endsWith(".sched") ||
      f.getName.endsWith(".ba") ||
      f.getName.endsWith(".bexe") ||
      f.getName.endsWith(".so") ||
      f.getName.endsWith(".out") ||
      f.getName.endsWith(".parse") ||
      f.getName.endsWith(".typecheck") ||
      f.getName.endsWith(".interpret")).foreach(f => f.delete())
  }
}
