package org.cse.homegrown

import org.cse.homegrown.utils.Utils

case class BlockWrapper(index: Long, timestamp: Long, private val content: Any, previousHash: Hash) {
  val data: Array[Byte] = Utils.serialize (content)
  val hash = SHA_256 (data)
  def content[T] (cls: Class[T]): T = Utils.deserialize (data, cls)
}
