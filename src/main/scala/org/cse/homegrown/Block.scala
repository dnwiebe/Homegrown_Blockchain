package org.cse.homegrown

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, ObjectInputStream, ObjectOutputStream}

object Block {
  private def serialize (data: Any): Array[Byte] = {
    val buf = new ByteArrayOutputStream ()
    val stream = new ObjectOutputStream (buf)
    stream.writeObject (data)
    stream.close ()
    buf.toByteArray
  }

  private def deserialize[T] (data: Array[Byte], cls: Class[T]): T = {
    val buf = new ByteArrayInputStream (data)
    val stream = new ObjectInputStream (buf)
    stream.readObject ().asInstanceOf[T]
  }
}

case class Block (index: Long, timestamp: Long, private val content: Any, previousHash: Hash) {
  import Block._

  val data: Array[Byte] = serialize (content)
  val hash = SHA_256 (data)
  def content[T] (cls: Class[T]): T = deserialize (data, cls)
}
