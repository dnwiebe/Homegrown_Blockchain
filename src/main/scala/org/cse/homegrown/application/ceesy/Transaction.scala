package org.cse.homegrown.application.ceesy

import org.cse.homegrown.blockchain.SHA_256
import org.cse.homegrown.utils.Utils

object Signature {
  def sign (obj: Any, privateKey: Array[Byte]): Signature = {
    val hash = SHA_256 (Utils.serialize (obj))
    new Signature (Utils.encrypt (hash.value, privateKey))
  }
}

case class Signature (bytes: Array[Byte]) {
  def verify (document: Any, publicKey: Array[Byte]): Boolean = {
    Utils.decrypt (bytes, publicKey) match {
      case None => false
      case Some (decrypted) => decrypted.sameElements (SHA_256 (Utils.serialize (document)).value)
    }
  }
}

case class Transaction (from: Array[Byte], to: Array[Byte], amount: Long, timestamp: Long)

object SignedTransaction {
  def pay (from: Array[Byte], to: Array[Byte], amount: Long, fromPrivateKey: Array[Byte]): SignedTransaction = {
    val transaction = Transaction (from, to, amount, System.currentTimeMillis())
    val signature = Signature.sign (transaction, fromPrivateKey)
    new SignedTransaction (transaction, signature)
  }
}

case class SignedTransaction (private val transaction: Transaction, private val signature: Signature) {
  val from: Array[Byte] = transaction.from
  val to: Array[Byte] = transaction.to
  val amount: Long = transaction.amount
  val timestamp: Long = transaction.timestamp

  def verify (): Boolean = {
    signature.verify (transaction, from)
  }
}
