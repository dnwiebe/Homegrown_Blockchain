package org.cse.homegrown.application.ceesy

import org.cse.homegrown.blockchain.BlockChain

import scala.concurrent.Future

class Wallet (blockchain: BlockChain, privateKey: Array[Byte], publicKey: Array[Byte]) {
  def makePayment (to: Array[Byte], amount: Long): Future[Boolean] = {
    throw new UnsupportedOperationException ()
  }

  def balance: Long = {
    throw new UnsupportedOperationException ()
  }
}
