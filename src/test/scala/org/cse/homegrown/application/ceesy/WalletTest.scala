package org.cse.homegrown.application.ceesy

import org.cse.homegrown.utils.{TestUtils, Utils}
import org.scalatest.path

import scala.util.Success

class WalletTest extends path.FunSpec {

  describe ("A BlockChain with a genesis block carrying a bunch of coins and a supply wallet") {
    val (initialPrivate, initialPublic) = TestUtils.makeKeyPair(12345)
    val ceesy = new Ceesy (1000000, initialPrivate, initialPublic)
    val initialWallet = new Wallet (ceesy, initialPrivate, initialPublic)

    describe ("with three regular wallets") {
      val (alicePrivate, alicePublic) = TestUtils.makeKeyPair(23456)
      val aliceWallet = new Wallet(ceesy, alicePrivate, alicePublic)
      val (bobPrivate, bobPublic) = TestUtils.makeKeyPair(34567)
      val bobWallet = new Wallet(ceesy, bobPrivate, bobPublic)
      val (minerPrivate, minerPublic) = TestUtils.makeKeyPair (45768)
      val minerWallet = new Wallet (ceesy, minerPrivate, minerPublic)

      describe("when a series of payments are made") {
        val initialToAlice = initialWallet.makePayment(alicePublic, 1000)
        val initialToBob = initialWallet.makePayment(bobPublic, 750)
        val aliceToBob = aliceWallet.makePayment(bobPublic, 200)

        describe ("and a block mined") {
          val miner = new Miner (ceesy, minerPublic)
          miner.verifyOutstandingPayments()

          it ("all payments are verified, wallets have expected balances") {
            assert(initialToAlice.value === Some(Success(true)))
            assert(initialToBob.value === Some(Success(true)))
            assert(aliceToBob.value === Some(Success(true)))

            assert (initialWallet.balance === 1000000 - 1000 - 750)
            assert (aliceWallet.balance === 1000 - 200)
            assert (bobWallet.balance === 750 + 200)
            assert (minerWallet.balance === Miner.TOKENS_PER_BLOCK)
          }
        }
      }
    }
  }
}
