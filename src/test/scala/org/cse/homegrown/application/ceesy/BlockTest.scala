package org.cse.homegrown.application.ceesy

import org.cse.homegrown.utils.TestUtils.OffsetTimestamper
import org.cse.homegrown.utils.{Nonce, TestUtils, Timestamper}
import org.scalatest.path

class BlockTest extends path.FunSpec {

  describe ("A Block with two transactions in it") {
    implicit val timestamper: Timestamper = new OffsetTimestamper ()
    val (alicePrivate, alicePublic) = TestUtils.makeKeyPair (12345)
    val (bobPrivate, bobPublic) = TestUtils.makeKeyPair (23456)
    val (_, validatorPublic) = TestUtils.makeKeyPair (34567)
    val (aliceToBob, _) = SignedTransaction.pay (alicePublic, bobPublic, 100, alicePrivate)
    val (bobToAlice, _) = SignedTransaction.pay (bobPublic, alicePublic, 100, bobPrivate)
    val transactions = Array (VerifiedTransaction (aliceToBob), VerifiedTransaction (bobToAlice))

    val before = timestamper.stamp ()
    val subject = Block (transactions, validatorPublic)
    val after = timestamper.stamp ()

    it ("has the expected data in it") {
      assert (subject.transactions === transactions)
      assert (subject.timestamp >= before)
      assert (subject.timestamp <= after)
      assert (subject.validator === validatorPublic)
      assert (subject.nonce.bytes.isEmpty === true)
    }

    it ("can be made Pretty in a reasonable number of tries") {
      val limit = 1 << (Block.REQUIRED_ZEROS + 2) // four times the mean
      var value = 0
      var prettyFound = false
      while (!prettyFound && (value < limit)) {
        val nonce = new Nonce (Array (
          ((value >> 24) & 0xFF).asInstanceOf[Byte],
          ((value >> 16) & 0xFF).asInstanceOf[Byte],
          ((value >> 8) & 0xFF).asInstanceOf[Byte],
          ((value >> 0) & 0xFF).asInstanceOf[Byte]
        ))
        val attempt = subject.withNonce (nonce)
        prettyFound = attempt.isPretty
        value += 1
      }
      assert (prettyFound === true)
    }
  }
}
