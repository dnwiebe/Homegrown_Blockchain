package org.cse.homegrown.blockchain

import org.cse.homegrown.application.ceesy.PlainData
import org.cse.homegrown.utils.Utils

case class BlockWrapper(index: Long, timestamp: Long, private val content: Any, previousHash: Hash) {
  val data: PlainData = Utils.serialize (content)
  val hash = SHA_256 (data)
  def content[T] (cls: Class[T]): T = Utils.deserialize (data, cls)
}
