package pipedsl

import java.io.File

import org.scalatest.funsuite.AnyFunSuite



class MainSuite extends AnyFunSuite {


  private val histFolder = "src/test/tests/histogram"
  private val histFile = histFolder + "/histogram.pdl"
  private val histBram = histFolder + "/histogram_bram.pdl"
  private val histShort = histFolder + "/histogram_short.pdl"
  private val histInputs = histFolder + "/memInputs"
  private val inputH = histInputs + "/h"
  private val inputF = histInputs + "/f"
  private val inputW = histInputs + "/w"
  private val inputMap = Map("h" -> inputH, "f" -> inputF, "w" -> inputW)

  test("Histogram Parse Test") {
    testParse(new File(histFolder),
      new File(histFile))
  }

  test("Histogram Typecheck Test") {
    testTypecheck(new File(histFolder),
      new File(histFile))
  }

  test("Histogram Compilation Test") {
    testBlueSpecCompile(new File(histFolder),
      new File(histFile),
      None,
      inputMap
    )
  }
  
  test("Histogram Simulation Test") {
    testBlueSpecSim(new File(histFolder),
      new File(histFile),
      None,
      inputMap
    )
  }

  test("Histogram BRAM Parse Test") {
    testParse(new File(histFolder),
      new File(histBram))
  }

  test("Histogram BRAM Typecheck Test") {
    testTypecheck(new File(histFolder),
      new File(histBram))
  }

  test("Histogram BRAM Compilation Test") {
    testBlueSpecCompile(new File(histFolder),
      new File(histBram),
      None,
      inputMap
    )
  }

  test("Histogram BRAM Simulation Test") {
    testBlueSpecSim(new File(histFolder),
      new File(histBram),
      None,
      inputMap
    )
  }

  test("Histogram SHORT Parse Test") {
    testParse(new File(histFolder),
      new File(histShort))
  }

  test("Histogram SHORT Typecheck Test") {
    testTypecheck(new File(histFolder),
      new File(histShort))
  }

  test("Histogram SHORT Compilation Test") {
    testBlueSpecCompile(new File(histFolder),
      new File(histShort),
      None,
      inputMap
    )
  }

  test("Histogram SHORT Simulation Test") {
    testBlueSpecSim(new File( histFolder),
      new File(histShort),
      None,
      inputMap)
  }

  private val matpowFolder = "src/test/tests/matpow"
  private val matpowFile = matpowFolder + "/matpow.pdl"
  private val matpowBram = matpowFolder + "/matpow_bram.pdl"
  private val matpowAlt = matpowFolder + "/matpow_alt.pdl"
  private val matpowInputs = matpowFolder + "/memInputs"
  private val matpowMap = Map("a" -> (matpowInputs + "/a_2"),
    "x" -> (matpowInputs + "/x"),
    "r" -> (matpowInputs + "/r"),
    "c" -> (matpowInputs + "/c")
  )

  test("Matrix Power Parse Test") {
    testParse(new File(matpowFolder),
      new File(matpowFile))
  }
  
  test("Matrix Power Typecheck Test") {
    testTypecheck(new File(matpowFolder),
      new File(matpowFile))
  }

  test("Matrix Power Compilation Test") {
    testBlueSpecCompile(new File(matpowFolder),
      new File(matpowFile),
      None,
      matpowMap
    )
  }
  
  test("Matrix Power Simulation Test") {
    testBlueSpecSim(new File(matpowFolder),
      new File(matpowFile),
      None,
      matpowMap
    )
  }

  test("Matrix Power BRAM Parse Test") {
    testParse(new File(matpowFolder),
      new File(matpowBram))
  }

  test("Matrix Power BRAM Typecheck Test") {
    testTypecheck(new File(matpowFolder),
      new File(matpowBram))
  }

  test("Matrix Power BRAM Compilation Test") {
    testBlueSpecCompile(new File(matpowFolder),
      new File(matpowBram),
      None,
      matpowMap
    )
  }

  test("Matrix Power BRAM Simulation Test") {
    testBlueSpecSim(new File(matpowFolder),
      new File(matpowBram),
      None,
      matpowMap
    )
  }
  
  test("Matrix Power ALT Parse Test") {
    testParse(new File(matpowFolder),
      new File(matpowAlt))
  }

  test("Matrix Power ALT Typecheck Test") {
    testTypecheck(new File(matpowFolder),
      new File(matpowAlt))
  }

  test("Matrix Power ALT Compilation Test") {
    testBlueSpecCompile(new File( matpowFolder),
      new File(matpowAlt),
      None,
      matpowMap
    )
  }

  test("Matrix Power ALT Simulation Test") {
    testBlueSpecSim(new File(matpowFolder),
      new File(matpowAlt),
      None,
      matpowMap
    )
  }

  private val multiFolder = "src/test/tests/multiExec"
  private val multiFile = multiFolder + "/multiexec.pdl"
  private val multiAlt = multiFolder + "/multiexec_alt.pdl"
  private val multiSplit = multiFolder + "/multiexec_split.pdl"
  private val multiInputs = multiFolder + "/memInputs"
  private val memMap = Map(
    "i" -> (multiInputs + "/i"),
    "r" -> (multiInputs + "/r")
  )

  test("Multiple Execution Parse Test") {
    testParse(new File(multiFolder),
      new File(multiFile))
  }  
  
  test("Multiple Execution Typecheck Test") {
    testTypecheck(new File(multiFolder),
      new File(multiFile))
  }

  test("Multiple Execution CompilationTest") {
    testBlueSpecCompile(new File(multiFolder),
      new File(multiFile),
      None,
      memMap
    )
  }
  
  test("Multiple Execution Simulation Test") {
    testBlueSpecSim(new File(multiFolder),
      new File(multiFile),
      None,
      memMap
    )
  }

  test("Multiple Execution OOO WB Parse Test") {
    testParse(new File(multiFolder),
      new File(multiFile))
  }
  
  test("Multiple Execution OOO WB Typecheck Test") {
    testTypecheck(new File(multiFolder),
      new File(multiFile))
  }

  test("Multiple Execution OOO WB Compilation Test") {
    testBlueSpecCompile(new File(multiFolder),
      new File(multiAlt),
      None,
      memMap
    )
  }

  test("Multiple Execution OOO WB Simulation Test") {
    testBlueSpecSim(new File(multiFolder),
      new File(multiAlt),
      None,
      memMap
    )
  }

  test("Multiple Execution Split Parse Test") {
    testParse(new File(multiFolder),
      new File(multiSplit))
  }

  test("Multiple Execution Split Typecheck Test") {
    testTypecheck(new File(multiFolder),
      new File(multiSplit))
  }

  test("Multiple Execution Split Compilation Test") {
    testBlueSpecCompile(new File(multiFolder),
      new File(multiSplit),
      None,
      memMap
    )
  }

  test("Multiple Execution Split Simulation Test") {
    testBlueSpecSim(new File(multiFolder),
      new File(multiSplit),
      None,
      memMap
    )
  }



  val testDir = new File("src/test/tests/typecheckTests")
    for (file <- testDir.listFiles().filter(f => f.isFile)) {
      test(file.toString()) { 
        testTypecheck(testDir, file)
      }
    }
  

}
