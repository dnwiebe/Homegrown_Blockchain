package org.cse.homegrown.utils

object Utils {

  val HEX_DIGITS = Array ('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F')

  def stringFromByte (byte: Byte): String = {
    val buf = new StringBuilder ()
    buf.append (HEX_DIGITS ((byte >> 4) & 0x0F))
    buf.append (HEX_DIGITS (byte & 0x0F))
    buf.toString ()
  }

  def stringFromBytes (bytes: Seq[Byte]): String = {
    bytes.map (stringFromByte).mkString ("")
  }
}
