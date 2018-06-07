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

trait Transaction {
  val transaction: TransactionGuts
  def from: PublicKey = transaction.from
  def to: PublicKey = transaction.to
  def amount: Long = transaction.amount
  def timestamp: Long = transaction.timestamp
}

case class TransactionGuts(from: PublicKey, to: PublicKey, amount: Long, timestamp: Long)

object SignedTransaction {
  var timestamper: Timestamper = new RealTimestamper ()

  def pay (from: PublicKey, to: PublicKey, amount: Long, fromPrivateKey: PrivateKey): (SignedTransaction, Future[Boolean]) = {
    val transaction = TransactionGuts (from, to, amount, System.currentTimeMillis())
    val signature = Signature.sign (transaction, fromPrivateKey)
    val verifyPromise = Promise[Boolean] ()
    val result = new SignedTransaction (transaction, signature, verifyPromise)
    (result, verifyPromise.future)
  }

  def miningReward (miner: PublicKey): SignedTransaction = {
    val transaction = TransactionGuts (new PublicKey (Array ()), miner, Miner.TOKENS_PER_BLOCK, timestamper.stamp ())
    val signature = new Signature (new ByteSeq (Array ()))
    val verifyPromise = Promise[Boolean] ()
    new SignedTransaction (transaction, signature, verifyPromise)
  }
}

case class SignedTransaction (transaction: TransactionGuts, signature: Signature, verifyPromise: Promise[Boolean]) extends Transaction {
  def verifySignature(): Boolean = {
    signature.verify (transaction, from)
  }
}

object VerifiedTransaction {
  var timestamper: Timestamper = new RealTimestamper ()

  def apply (signed: SignedTransaction): VerifiedTransaction = {
    VerifiedTransaction (signed.transaction, signed.signature)
  }

  def miningReward (miner: PublicKey): VerifiedTransaction = {
    val transaction = TransactionGuts (new PublicKey (Array ()), miner, Miner.TOKENS_PER_BLOCK, timestamper.stamp ())
    val signature = new Signature (new ByteSeq (Array ()))
    val verifyPromise = Promise[Boolean] ()
    VerifiedTransaction (new SignedTransaction (transaction, signature, verifyPromise))
  }
}

case class VerifiedTransaction (transaction: TransactionGuts, private val signature: Signature) extends Transaction