package org.cse.homegrown.application.ceesy

import org.cse.homegrown.blockchain.BlockWrapper
import org.cse.homegrown.utils.{Nonce, PublicKey}

import scala.collection.immutable.HashMap
import scala.concurrent.Promise
import scala.util.{Random, Success}

object Miner {
  val TOKENS_PER_BLOCK = 10
}

class Miner (ceesy: Ceesy, minerPublic: PublicKey) {

  type Balances = HashMap[PublicKey, Long]

  def verifyOutstandingPayments (): Boolean = {
    val verificationResults = verifySignedTransactions ()
    val block = makeBlock (verificationResults)
    val prettyBlock = prettify (block)
    ceesy.chain.add (prettyBlock)
    verificationResults.foreach {result =>
      val (xactn, valid) = result
      xactn.verifyPromise.complete (Success (valid))
    }
    true
  }

  private def verifySignedTransactions (): List[(SignedTransaction, Boolean)] = {
    val signedTransactions = ceesy.takePendingTransactions()
    signedTransactions.foldLeft((
      List[(SignedTransaction, Boolean)](),
      computeBalances(ceesy.chain.latest, new HashMap())
    )) { (soFar, xactn) =>
      val (verifyPromises, balances) = soFar
      val (updatedBalances, result) = verifyTransaction(xactn, balances)
      val updatedResults = (xactn, result) :: verifyPromises
      (updatedResults, updatedBalances)
    }._1
  }

  private def makeBlock (verificationResults: List[(SignedTransaction, Boolean)]): Block = {
    val verifiedTransactions = verificationResults.flatMap {pair =>
      pair match {
        case (xactn, true) => Some (VerifiedTransaction (xactn))
        case (xactn, false) => None
      }
    }
    Block ((VerifiedTransaction.miningReward(minerPublic) :: verifiedTransactions).toArray, minerPublic)
  }

  private def prettify (block: Block): Block = {
    var toPrettify = block
    val random = new Random ()
    while (!toPrettify.isPretty) {
      val nonce = chooseNonce (random)
      toPrettify = toPrettify.withNonce (nonce)
    }
    toPrettify
  }

  private def chooseNonce (random: Random): Nonce = {
    val value = random.nextInt
    val result = new Array[Byte] (4)
    (0 until 4).foreach {idx => result (idx) = ((value >> (idx * 4)) & 0xFF).asInstanceOf[Byte]}
    new Nonce (result)
  }

  private def computeBalances (blockWrapper: BlockWrapper, balances: Balances): Balances = {
    val block = blockWrapper.content (classOf[Block])
    val adjustedBalances = block.transactions.foldLeft (balances) {(soFar, transaction) =>
      val afterFrom = recordIncrement (balances, transaction.from, -transaction.amount)
      recordIncrement (afterFrom, transaction.to, transaction.amount)
    }
    if (blockWrapper.previousHash.bytes.isEmpty) {
      adjustedBalances
    }
    else {
      computeBalances (ceesy.chain.block (blockWrapper.previousHash).get, adjustedBalances)
    }
  }

  private def recordIncrement (balances: Balances, publicKey: PublicKey, increment: Long): Balances = {
    balances.get (publicKey) match {
      case None => balances + (publicKey -> increment)
      case Some (balance) => balances + (publicKey -> (balance + increment))
    }
  }

  private def verifyTransaction (transaction: SignedTransaction, balances: Balances): (Balances, Boolean) = {
    if (!transaction.verifySignature ()) {return (balances, false)}
    val afterFrom = recordIncrement (balances, transaction.from, -transaction.amount)
    if (afterFrom (transaction.from) < 0) {return (balances, false)}
    val afterBoth = recordIncrement (afterFrom, transaction.to, transaction.amount)
    (afterBoth, true)
  }

  private def addBlock (block: Block): Unit = {
    ceesy.chain.add (block)
  }
}
