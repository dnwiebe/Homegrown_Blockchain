package org.cse.homegrown.application.ceesy

import org.cse.homegrown.blockchain.BlockChain
import org.cse.homegrown.utils.{TestUtils, Utils}
import org.scalatest.path

import scala.util.Success

class WalletTest extends path.FunSpec {

  describe ("A BlockChain with a genesis block carrying a bunch of coins and a supply wallet") {
    val (initialPrivate, initialPublic) = TestUtils.makeKeyPair(12345)
    val transaction = SignedTransaction.pay (Array (), initialPublic, 1000000, Array ())
    val genesisBlock = Block (Array (transaction), Array ())
    val chain = new BlockChain(genesisBlock)
    val initialWallet = new Wallet (chain, initialPrivate, initialPublic)

    describe ("with three regular wallets") {
      val (alicePrivate, alicePublic) = TestUtils.makeKeyPair(23456)
      val aliceWallet = new Wallet(chain, alicePrivate, alicePublic)
      val (bobPrivate, bobPublic) = TestUtils.makeKeyPair(34567)
      val bobWallet = new Wallet(chain, bobPrivate, bobPublic)
      val (minerPrivate, minerPublic) = TestUtils.makeKeyPair (45768)
      val minerWallet = new Wallet (chain, minerPrivate, minerPublic)

      describe("when a series of payments are made") {
        pending
        val initialToAlice = initialWallet.makePayment(alicePublic, 1000)
        val initialToBob = initialWallet.makePayment(bobPublic, 750)
        val aliceToBob = aliceWallet.makePayment(bobPublic, 200)

        describe ("and a block mined") {
          val miner = new Miner (chain, minerPublic)
          miner.verifyOutstandingPayments()

          it ("all payments are verified") {
            assert(initialToAlice.value === Some(Success(true)))
            assert(initialToBob.value === Some(Success(true)))
            assert(aliceToBob.value === Some(Success(true)))
          }

          it ("initial wallet has the expected balance") {
            assert (initialWallet.balance === 1000000 - 1000 - 750)
          }

          it ("Alice's wallet has the expected balance") {
            assert (aliceWallet.balance === 1000 - 200)
          }

          it ("Bob's wallet has the expected balance") {
            assert (bobWallet.balance === 750 + 200)
          }

          it ("Miner's wallet has the expected balance") {
            assert (minerWallet.balance === 3 * Miner.COINS_PER_TRANSACTION)
          }
        }
      }
    }
  }
}
