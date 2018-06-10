package org.cse.homegrown.application.ceesy

import org.cse.homegrown.blockchain.BlockWrapper
import org.cse.homegrown.utils.{PrivateKey, PublicKey}

import scala.concurrent.Future

class Wallet (ceesy: Ceesy, privateKey: PrivateKey, publicKey: PublicKey) {
  def makePayment (to: PublicKey, amount: Long): Future[Boolean] = {
    val (transaction, verifyFuture) = SignedTransaction.pay (publicKey, to, amount, privateKey)
    ceesy.pendTransaction (transaction)
    verifyFuture
  }

  def balance: Long = {
    balanceFrom (ceesy.latest)
  }

  private def balanceFrom (blockWrapper: BlockWrapper): Long = {
    val block = blockWrapper.content (classOf[Block])
    val blockBalance = block.transactions.foldLeft (0L) {(soFar, transaction) =>
      (transaction.from, transaction.to) match {
        case (from, _) if from == publicKey => soFar - transaction.amount
        case (_, to) if to == publicKey => soFar + transaction.amount
        case _ => soFar
      }
    }
    blockWrapper.previous (ceesy) match {
      case Some (previous) => blockBalance + balanceFrom (previous)
      case None => blockBalance
    }
  }
}
