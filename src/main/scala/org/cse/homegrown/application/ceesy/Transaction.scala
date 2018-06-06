package org.cse.homegrown.application.ceesy

import org.cse.homegrown.blockchain._
import org.cse.homegrown.utils._

import scala.concurrent.{Future, Promise}
import scala.util.Success

object Signature {
  def sign (obj: Any, privateKey: PrivateKey): Signature = {
    val hash = SHA_256 (Utils.serialize (obj))
    new Signature (Utils.encrypt (new PlainData (hash.bytes), privateKey))
  }
}

class Signature (bytes: ByteSeq) extends ByteSeq (bytes.bytes) {
  def verify (document: Any, publicKey: PublicKey): Boolean = {
    Utils.decrypt (new CryptData (bytes.bytes), publicKey) match {
      case None => false
      case Some (decrypted) => decrypted.bytes.sameElements (SHA_256 (Utils.serialize (document)).bytes)
    }
  }
}

case class Transaction (from: PublicKey, to: PublicKey, amount: Long, timestamp: Long)

object SignedTransaction {
  def pay (from: PublicKey, to: PublicKey, amount: Long, fromPrivateKey: PrivateKey): (SignedTransaction, Future[Boolean]) = {
    val transaction = Transaction (from, to, amount, System.currentTimeMillis())
    val signature = Signature.sign (transaction, fromPrivateKey)
    val verifyPromise = Promise[Boolean] ()
    val result = new SignedTransaction (transaction, signature, verifyPromise)
    (result, verifyPromise.future)
  }

  def miningReward (miner: PublicKey): SignedTransaction = {
    val transaction = Transaction (new PublicKey (Array ()), miner, Miner.TOKENS_PER_BLOCK, System.currentTimeMillis())
    val signature = new Signature (new ByteSeq (Array ()))
    val verifyPromise = Promise[Boolean] ()
    new SignedTransaction (transaction, signature, verifyPromise)
  }
}

case class SignedTransaction (private val transaction: Transaction, private val signature: Signature,
                              verifyPromise: Promise[Boolean]) {
  val from: PublicKey = transaction.from
  val to: PublicKey = transaction.to
  val amount: Long = transaction.amount
  val timestamp: Long = transaction.timestamp

  def verifySignature(): Boolean = {
    signature.verify (transaction, from)
  }
}
