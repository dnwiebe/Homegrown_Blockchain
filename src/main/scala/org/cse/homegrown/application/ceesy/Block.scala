package org.cse.homegrown.application.ceesy

import org.cse.homegrown.blockchain.SHA_256
import org.cse.homegrown.utils.Utils

object Block {
  val REQUIRED_ZEROS = 10

  def apply (transactions: Array[SignedTransaction], validator: Array[Byte]): Block = {
    new Block (transactions, System.currentTimeMillis(), validator, Array ())
  }
}

case class Block (transactions: Array[SignedTransaction], timestamp: Long, validator: Array[Byte], nonce: Array[Byte]) {
  import Block._

  def withNonce (nonce: Array[Byte]): Block = {
    new Block (transactions, timestamp, validator, nonce)
  }

  def isPretty: Boolean = {
    val hash = SHA_256 (Utils.serialize (this))
    hash.value.indices.find (idx => hash.value (idx) != 0) match {
      case None => true
      case Some (idx) => {
        var zerosRemaining = REQUIRED_ZEROS - (idx * 8)
        var byte = hash.value (idx)
        while ((byte & 0x80) == 0) {
          zerosRemaining -= 1
          byte = (byte << 1).asInstanceOf[Byte]
        }
        zerosRemaining <= 0
      }
    }
  }
}
