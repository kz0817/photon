package photon

import java.nio.ByteBuffer

object Util {
  def dump(buff: ByteBuffer) {
    val origLimit = buff.limit()
    val origPosition = buff.position()
    buff.rewind()
    val size: Int = buff.limit
    logger.info(s"[Dump] Size: $size, limit: $origLimit, pos: $origPosition")

    val NUM_ROWS = 16

    def padding(line: String): String = {
      val fixedWidth = NUM_ROWS*2 + (NUM_ROWS-1)
      val numPad = fixedWidth - line.length
      if (numPad == 0)
        line
      else
        line + ((1 to numPad) map(_ => " ") mkString(""))
    }

    val numLoops = (size + (NUM_ROWS-1)) / NUM_ROWS
    val numLastElem = size % NUM_ROWS
    val elemNumList = for (i <- 1 to numLoops) yield {
      if (i == numLoops && numLastElem > 0)
        numLastElem
      else
        NUM_ROWS
    }

    elemNumList.map { n =>
      for (i <- 1 to n) yield buff.get
    }
    .foreach {chunk =>
      val hexList = chunk map {d => f"$d%02x"} mkString(" ")
      val charList = chunk map {
        _ match {
          case d if d >= 0x20 && d<= 0x7e => f"$d%c"
          case _ => '.'
        }
      } mkString("")

      println(padding(hexList) + "  " + charList)
    }
    buff.position(origPosition)
    buff.limit(origLimit)
  }

  def isAscii(ch: Byte): Boolean = { ch >= 0x20 && ch <= 0x7e }
}

