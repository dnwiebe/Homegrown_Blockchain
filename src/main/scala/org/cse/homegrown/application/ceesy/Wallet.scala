package org.cse.homegrown.application.ceesy

import scala.concurrent.Future

class Wallet (ceesy: Ceesy, privateKey: PrivateKey, publicKey: PublicKey) {
  def makePayment (to: PublicKey, amount: Long): Future[Boolean] = {
    val (transaction, verifyFuture) = SignedTransaction.pay (publicKey, to, amount, privateKey)
    ceesy.pendTransaction (transaction)
    verifyFuture
  }

  def balance: Long = {
    throw new UnsupportedOperationException ()
  }
}
