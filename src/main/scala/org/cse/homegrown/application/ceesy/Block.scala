package org.cse.homegrown.application.ceesy

import org.cse.homegrown.blockchain.SHA_256
import org.cse.homegrown.utils.{Nonce, PublicKey, Timestamper, Utils}

object Block {
  val REQUIRED_ZEROS = 10

  def apply (transactions: Array[VerifiedTransaction], validator: PublicKey) (implicit timestamper: Timestamper): Block = {
    new Block (transactions, timestamper.stamp (), validator, new Nonce (Array ()))
  }
}

case class Block (transactions: Array[VerifiedTransaction], timestamp: Long, validator: PublicKey, nonce: Nonce) {
  import Block._

  def withNonce (nonce: Nonce): Block = {
    new Block (transactions, timestamp, validator, nonce)
  }

  def isPretty: Boolean = {
    val hash = SHA_256 (Utils.serialize (this))
    hash.bytes.indices.find (idx => hash.bytes (idx) != 0) match {
      case None => true
      case Some (idx) => {
        var zerosRemaining = REQUIRED_ZEROS - (idx * 8)
        var byte = hash.bytes (idx)
        while ((byte & 0x80) == 0) {
          zerosRemaining -= 1
          byte = (byte << 1).asInstanceOf[Byte]
        }
        zerosRemaining <= 0
      }
    }
  }
}
