package org.cse.homegrown.utils

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, ObjectInputStream, ObjectOutputStream}

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

  def serialize (data: Any): Array[Byte] = {
    val buf = new ByteArrayOutputStream ()
    val stream = new ObjectOutputStream (buf)
    stream.writeObject (data)
    stream.close ()
    buf.toByteArray
  }

  def deserialize[T] (data: Array[Byte], cls: Class[T]): T = {
    val buf = new ByteArrayInputStream (data)
    val stream = new ObjectInputStream (buf)
    stream.readObject ().asInstanceOf[T]
  }

  def encrypt (plain: Array[Byte], key: Array[Byte]): Array[Byte] = {
    translateKey (key) ++ plain
  }

  def decrypt (crypt: Array[Byte], key: Array[Byte]): Option[Array[Byte]] = {
    if (crypt.take (key.length).sameElements (key)) {
      Some (crypt.drop (key.length))
    }
    else {
      None
    }
  }

  def translateKey (input: Array[Byte]): Array[Byte] = input.map (b => (b + 128).asInstanceOf[Byte])


}
