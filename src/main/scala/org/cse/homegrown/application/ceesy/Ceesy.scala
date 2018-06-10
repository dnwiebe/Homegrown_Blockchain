package org.cse.homegrown.application.ceesy

import org.cse.homegrown.blockchain.{BlockChain, BlockWrapper, Hash, ReadOnlyBlockChain}
import org.cse.homegrown.utils.{PrivateKey, PublicKey, RealTimestamper, Timestamper}

object Ceesy {
  implicit private val timestamper: Timestamper = new RealTimestamper ();
  private def makeChain (initialSupply: Long, initialPrivate: PrivateKey, initialPublic: PublicKey): BlockChain = {
    val (transaction, _) = SignedTransaction.pay (new PublicKey(Array ()), initialPublic, initialSupply, new PrivateKey (Array ()))
    val genesisBlock = Block (Array (VerifiedTransaction (transaction)), new PublicKey (Array ()))
    new BlockChain(genesisBlock)
  }
}

class Ceesy (initialSupply: Long, initialPrivate: PrivateKey, initialPublic: PublicKey) extends ReadOnlyBlockChain {
  import Ceesy._

  private val chain: BlockChain = makeChain (initialSupply, initialPrivate, initialPublic)
  var pendingTransactions: List[SignedTransaction] = Nil

  def pendTransaction (transaction: SignedTransaction): Unit = {
    pendingTransactions = transaction :: pendingTransactions
  }

  def takePendingTransactions (): List[SignedTransaction] = {
    val result = pendingTransactions.reverse
    pendingTransactions = Nil
    result
  }

  def block (hash: Hash): Option[BlockWrapper] = chain.block (hash)
  override def leaves(intervalMs: Long): Set[BlockWrapper] = chain.leaves (intervalMs)
  def latest: BlockWrapper = chain.latest
  def add (content: Any, previousHashOpt: Option[Hash] = None): Hash = chain.add (content, previousHashOpt)
  override def isValid: Boolean = chain.isValid
}
