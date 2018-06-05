package org.cse.homegrown.application.ceesy

import org.cse.homegrown.blockchain.BlockChain

object Ceesy {
  private def makeChain (initialSupply: Long, initialPrivate: PrivateKey, initialPublic: PublicKey): BlockChain = {
    val (transaction, _) = SignedTransaction.pay (new PublicKey(Array ()), initialPublic, initialSupply, new PrivateKey (Array ()))
    val genesisBlock = Block (Array (transaction), new PublicKey (Array ()))
    new BlockChain(genesisBlock)
  }
}

class Ceesy (initialSupply: Long, initialPrivate: PrivateKey, initialPublic: PublicKey) {
  import Ceesy._

  val chain: BlockChain = makeChain (initialSupply, initialPrivate, initialPublic)
  var pendingTransactions: List[SignedTransaction] = Nil

  def pendTransaction (transaction: SignedTransaction): Unit = {
    pendingTransactions = transaction :: pendingTransactions
  }

  def takePendingTransactions (): Array[SignedTransaction] = {
    val result = pendingTransactions.reverse.toArray
    pendingTransactions = Nil
    result
  }
}
