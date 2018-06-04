package org.cse.homegrown.application.ceesy

import org.cse.homegrown.blockchain.BlockChain

object Miner {
  val COINS_PER_TRANSACTION = 10
}

class Miner (chain: BlockChain, minerPublic: Array[Byte]) {

  def verifyOutstandingPayments (): Boolean = {
    throw new UnsupportedOperationException ()
  }
}
