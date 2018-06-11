package org.cse.homegrown.application.ceesy

import org.cse.homegrown.blockchain.{BlockWrapper, Hash}
import org.cse.homegrown.utils.{Nonce, PublicKey, RealTimestamper, Timestamper}

import scala.collection.immutable.HashMap
import scala.util.{Random, Success}

object Miner {
  val TOKENS_PER_BLOCK = 10
}

class Miner (ceesy: Ceesy, minerPublic: PublicKey) {

  type Balances = HashMap[PublicKey, Long]

  implicit var timestamper: Timestamper = new RealTimestamper ()

  def verify () {
    val (lastValidBlockWrapper, balances) = verifyChain ()
    val signedTransactions = ceesy.takePendingTransactions()
    val verificationResults = verifySignedTransactions (signedTransactions, balances)
    val block = makeBlock (verificationResults)
    val prettyBlock = prettify (block)

    ceesy.add (prettyBlock, Some (lastValidBlockWrapper.hash))

    verificationResults.foreach {result =>
      val (xactn, valid) = result
      xactn.verifyPromise.complete (Success (valid))
    }
  }

  private def verifyChain (): (BlockWrapper, Balances) = {
    val blockWrapper = ceesy.latest
    val balances = computeBalances (blockWrapper, new HashMap ())
    var lastValidBlockWrapper = verifyBalances (blockWrapper, balances)
    lastValidBlockWrapper = verifyRewards (lastValidBlockWrapper)
    lastValidBlockWrapper = verifyHashesAndTimestamps (lastValidBlockWrapper)
    (lastValidBlockWrapper, balances)
  }

  private def verifySignedTransactions (signedTransactions: List[SignedTransaction], balances: Balances): List[(SignedTransaction, Boolean)] = {
    signedTransactions.foldLeft((List[(SignedTransaction, Boolean)](), balances)) { (soFar, xactn) =>
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
      computeBalances (ceesy.block (blockWrapper.previousHash).get, adjustedBalances)
    }
  }

  private def verifyBalances (blockWrapper: BlockWrapper, balances: Balances): BlockWrapper = {
    if (blockWrapper.previousHash.bytes.isEmpty) {return blockWrapper}
    val block = blockWrapper.content(classOf[Block])
    val (adjustedBalances, valid) = block.transactions.foldLeft((balances, true)) { (soFar, transaction) =>
      val (balances, valid) = soFar
      val fromAfter = balances(transaction.from)
      val toAfter = balances(transaction.to)
      val fromBefore = fromAfter + transaction.amount
      val toBefore = toAfter - transaction.amount
      val beforeBalances = balances + (transaction.from -> fromBefore) + (transaction.to -> toBefore)
      if ((fromAfter < 0L) || (fromBefore < 0L) || (toAfter < 0L) || (toBefore < 0L)) {
        (beforeBalances, false)
      }
      else {
        (beforeBalances, valid)
      }
    }
    val previousBlockWrapper = ceesy.block (blockWrapper.previousHash).get
    val previousResult = verifyBalances (previousBlockWrapper, adjustedBalances)
    if ((previousResult.hash == previousBlockWrapper.hash) && valid) {
      blockWrapper
    }
    else {
      previousResult
    }
  }

  private def verifyRewards (blockWrapper: BlockWrapper): BlockWrapper = {
    if (blockWrapper.previousHash.bytes.isEmpty) {return blockWrapper}
    val block = blockWrapper.content (classOf[Block])
    val from = new PublicKey(Array ())
    val to = block.validator
    val rewardPayments = block.transactions.filter {t => (t.from == from) && (t.to == to)}
    var valid = rewardPayments.length == 1
    if (valid) {
      val rewardPayment = rewardPayments.head
      valid &= rewardPayment.transaction.amount == Miner.TOKENS_PER_BLOCK
    }
    val previousBlockWrapper = ceesy.block (blockWrapper.previousHash).get
    val previousResult = verifyRewards (previousBlockWrapper)
    if ((previousResult.hash == previousBlockWrapper.hash) && valid) {
      blockWrapper
    }
    else {
      previousResult
    }
  }

  private def verifyHashesAndTimestamps (blockWrapper: BlockWrapper): BlockWrapper = {
    if (blockWrapper.previousHash.bytes.isEmpty) {return blockWrapper}
    val block = blockWrapper.content (classOf[Block])
    var valid = true
    valid &= block.isPretty
    val earliestTimestamp = block.transactions.map {x => x.timestamp}.min
    val latestTimestamp = block.transactions.map {x => x.timestamp}.max
    valid &= (block.timestamp >= latestTimestamp)
    val previousBlockWrapper = ceesy.block (blockWrapper.previousHash).get
    val previousResult = verifyHashesAndTimestamps (previousBlockWrapper)
    if (previousResult.hash != previousBlockWrapper.hash) {return previousResult}
    valid &= (previousBlockWrapper.timestamp <= blockWrapper.timestamp)
    valid &= (previousBlockWrapper.timestamp <= earliestTimestamp)
    if (valid) {
      blockWrapper
    }
    else {
      previousResult
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
}
