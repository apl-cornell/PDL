import java.io.File
import java.nio.file.Paths

import org.apache.commons.io.{FileUtils, FilenameUtils}

import scala.reflect.io.Directory
import scala.sys.process._

package object pipedsl {
  val pathToBluespecScript = "bin/runbsc"

  def getListOfFiles(dir: String, extension: String):List[File] = {
    val d = new File(dir)
    if (d.exists && d.isDirectory) {
      d.listFiles.filter(_.isFile).toList.filter { f =>
        f.getName.endsWith(extension)
      }
    } else {
      List[File]()
    }
  }

  def getListOfTests(dir: String): List[File] = {
    getListOfFiles(dir, ".pdl")
  }

  def getListOfSims(dir: String): List[File] = {
    getListOfFiles(dir + "/solutions", ".simsol")
  }

  def getTestName(f: File): String = {
    FilenameUtils.removeExtension(f.getName)
  }

  def getSimFile(dir: File, name: String): File = {
    new File(Paths.get(dir.getAbsolutePath, "solutions", name + ".simsol").toString)
  }

  def testParse(testDir: File, inputFile: File): Unit = {
    Main.parse(debug = false, printOutput = true, inputFile, testDir)
    val success = compareFiles(testDir, inputFile, "parse")
    deleteGeneratedFiles(testDir)
    assert(success)
  }

  def testTypecheck(testDir: File, inputFile: File): Boolean = {
    var doesTypecheck: Boolean = false
    try {
      Main.runPasses(printOutput = true, inputFile, testDir, port_warn = false)
      doesTypecheck = true
    } catch {
      case _: Throwable => ()
    }
    val success = compareFiles(testDir, inputFile, "typecheck")
    deleteGeneratedFiles(testDir)
    assert(success)
    return doesTypecheck
  }

  def testBlueSpecCompile(testDir: File, inputFile: File, addrLockMod: Option[String] = None, memInit: Map[String, String]): Unit = {
    val _ = (pathToBluespecScript + " c " + testDir.getAbsolutePath).!!
    Main.gen(testDir, inputFile, printStgInfo = false, debug = false, memInit, portWarn = false, addrLockMod)
    val exit = (pathToBluespecScript + " v " + testDir.getAbsolutePath).!
    deleteGeneratedFiles(testDir)
    deleteBSVFiles(testDir, memInit)
    assert(exit == 0)
  }

  def testBlueSpecSim(testDir: File, inputFile: File, addrLockMod: Option[String] = None, memInit: Map[String, String], simFile: Option[String] = None): Unit = {
    val _ = (pathToBluespecScript + " c " + testDir.getAbsolutePath).!!
    Main.gen(testDir, inputFile, printStgInfo = false, debug = false, memInit, portWarn = false, addrLockMod)
    val exit = (pathToBluespecScript + " s " + testDir.getAbsolutePath + " " + FilenameUtils.getBaseName(inputFile.getName) + ".sim").!
    val success = exit == 0 && compareFiles(testDir, inputFile, "sim", simFile)
    deleteGeneratedFiles(testDir)
    deleteBSVFiles(testDir, memInit)
    assert(success)
  }

  def compareFiles(testDir: File, inputFile: File, fileExtension: String, solName: Option[String] = None): Boolean = {
    val outputName = FilenameUtils.getBaseName(inputFile.getName) + "." + fileExtension
    val outputFile = new File(Paths.get(testDir.getAbsolutePath, outputName).toString)
    val expected = if (solName.isDefined) {
      new File(Paths.get(testDir.getAbsolutePath, "solutions", solName.get).toString)
    } else {
      new File(Paths.get(testDir.getAbsolutePath, "solutions", outputName + "sol").toString)
    }
    return FileUtils.contentEqualsIgnoreEOL(outputFile, expected, null);
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
      f.getName.endsWith(".sim") ||
      f.getName.endsWith(".interpret")).foreach(f => f.delete())
  }

  def deleteBSVFiles(testDir: File, memMap: Map[String, String]): Unit = {
    memMap.values.foreach(memPath =>
      new File(Paths.get(testDir.getAbsolutePath, FilenameUtils.getName(memPath)).toString).delete())
    new Directory(new File(Paths.get(testDir.getAbsolutePath, "Circuit_sim").toString)).deleteRecursively()
    new Directory(new File(Paths.get(testDir.getAbsolutePath, "Circuit_verilog").toString)).deleteRecursively()
    new Directory(new File(Paths.get(testDir.getAbsolutePath, "Circuit_sim").toString)).delete()
    new Directory(new File(Paths.get(testDir.getAbsolutePath, "Circuit_verilog").toString)).delete()
  }
}
