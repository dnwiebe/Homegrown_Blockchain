package org.cse.homegrown.application.ceesy

import org.cse.homegrown.blockchain.BlockWrapper

import scala.collection.immutable.HashMap
import scala.concurrent.Promise
import scala.util.{Random, Success}

object Miner {
  val COINS_PER_TRANSACTION = 10
}

class Miner (ceesy: Ceesy, minerPublic: PublicKey) {

  type Balances = HashMap[PublicKey, Long]

  def verifyOutstandingPayments (): Boolean = {
    val transactions = ceesy.takePendingTransactions ()
    val (promisePairs, rewards, balances) = transactions.foldLeft ((
        List[(Promise[Boolean], Boolean)] (),
        List[SignedTransaction] (),
        computeBalances (ceesy.chain.latest, new HashMap ())
    )) {(soFar, xactn) =>
      val (verifyPromises, miningRewards, balances) = soFar
      val (updatedBalances, result) = verifyTransaction (xactn, balances)
      val updatedRewards = SignedTransaction.miningReward (minerPublic) :: miningRewards
      val updatedPromises = (xactn.verifyPromise, result) :: verifyPromises
      (updatedPromises, updatedRewards, updatedBalances)
    }
    val block = Block (transactions ++ rewards, minerPublic)
    val prettyBlock = prettify (block)
    ceesy.chain.add (prettyBlock)
    promisePairs.foreach {pair =>
      val (promise, result) = pair
      promise.complete (Success (result))
    }
    true
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
