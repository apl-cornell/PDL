#!/bin/sh
exec scala "$0" "$@"
!#

import java.io.{BufferedWriter, File, FileWriter}


object GenTestFiles {
    def main(args: Array[String]) {
      val fileName = args(0)
      //size of the address in bits
      val memAddrSize = args(1)
      //size of the contents of memory in bits
      val memType = args(2)
      //random or constant or range
      val genType = args(3)
      val file = new File(fileName)
      val bw = new BufferedWriter(new FileWriter(file))
      val r = scala.util.Random
      //Max possible value that the int can be + 1
      val maxInt = if(args.size == 5) args(4).toLong else scala.math.pow(2, memType.toInt).asInstanceOf[Long]
      val str = new StringBuilder()
      var a = 0;
      val hexDigits = memType.toInt / 4 + (if(memType.toInt % 4 == 0) 0 else 1)
      //max possible address + 1
      val maxMem = scala.math.pow(2, memAddrSize.toInt).asInstanceOf[Long]
      var counter = 0L
      while (a < maxMem) {
        if (genType.equals("rand")) str.addAll(r.nextLong(maxInt).toHexString takeRight hexDigits)
        if (genType.equals("constant")) str.addAll(args(4).toLong.toHexString takeRight hexDigits)
        if (genType.equals("range")) if (counter < args(4).toLong) str.addAll(counter.toHexString takeRight hexDigits); counter = counter + 1L
        str.addOne('\n')
        a = a + 1
      }
      bw.write(str.toString())
      bw.close()
  }
}

GenTestFiles.main(args)
