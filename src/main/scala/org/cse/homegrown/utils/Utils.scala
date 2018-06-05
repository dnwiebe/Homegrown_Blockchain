package org.cse.homegrown.utils

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, ObjectInputStream, ObjectOutputStream}

import org.cse.homegrown.application.ceesy._

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

  def serialize (data: Any): PlainData = {
    val buf = new ByteArrayOutputStream ()
    val stream = new ObjectOutputStream (buf)
    stream.writeObject (data)
    stream.close ()
    new PlainData (buf.toByteArray)
  }

  def deserialize[T] (data: PlainData, cls: Class[T]): T = {
    val buf = new ByteArrayInputStream (data.bytes)
    val stream = new ObjectInputStream (buf)
    stream.readObject ().asInstanceOf[T]
  }

  def encrypt (plain: PlainData, key: Key): CryptData = {
    new CryptData (translateKey (key.bytes) ++ plain.bytes)
  }

  def decrypt (crypt: CryptData, key: Key): Option[PlainData] = {
    if (crypt.bytes.take (key.bytes.length).sameElements (key.bytes)) {
      Some (new PlainData (crypt.bytes.drop (key.bytes.length)))
    }
    else {
      None
    }
  }

  def translateKey (input: Array[Byte]): Array[Byte] = input.map (b => (b + 128).asInstanceOf[Byte])

  def translateKey (input: PublicKey): PrivateKey = new PrivateKey (translateKey (input.bytes))
  def translateKey (input: PrivateKey): PublicKey = new PublicKey (translateKey (input.bytes))
}
