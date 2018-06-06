package org.cse.homegrown.application.ceesy

import org.cse.homegrown.utils.TestUtils
import org.scalatest.path

class CeesyTest extends path.FunSpec {
  describe ("A brand-new Ceesy") {
    val (privateKey, publicKey) = TestUtils.makeKeyPair(12345)
    val subject = new Ceesy (123456L, privateKey, publicKey)

    describe ("asked for pending transactions") {
      val result = subject.takePendingTransactions()

      it ("can't supply any") {
        assert (result.isEmpty === true)
      }
    }

    describe ("queried about initial balance") {
      val wallet = new Wallet (subject, privateKey, publicKey)
      val result = wallet.balance

      it ("shows the initial balance") {
        assert (result === 123456L)
      }
    }

    describe ("given two transactions") {
      val (privateA, publicA) = TestUtils.makeKeyPair (23456)
      val walletA = new Wallet (subject, privateA, publicA)
      val (privateB, publicB) = TestUtils.makeKeyPair (34567)
      val walletB = new Wallet (subject, privateB, publicB)

      walletA.makePayment (publicB, 101L)
      walletB.makePayment (publicA, 102L)

      describe ("and asked for pending transactions") {
        val result = subject.takePendingTransactions ()

        it ("gets two of them") {
          val xactn1 = result.head
          assert (xactn1.from === publicA)
          assert (xactn1.to === publicB)
          assert (xactn1.amount === 101L)

          val xactn2 = result.tail.head
          assert (xactn2.from === publicB)
          assert (xactn2.to === publicA)
          assert (xactn2.amount === 102L)
        }
      }
    }
  }
}
