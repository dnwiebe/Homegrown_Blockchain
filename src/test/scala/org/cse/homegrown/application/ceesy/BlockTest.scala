package org.cse.homegrown.application.ceesy

import org.cse.homegrown.utils.Utils
import org.scalatest.path

class BlockTest extends path.FunSpec {

  describe ("A Block with two transactions in it") {
    val (alicePrivate, alicePublic) = makeKeyPair (91, 82, 35, 9, 32, 12)
    val (bobPrivate, bobPublic) = makeKeyPair (35, 9, 32, 12, 91, 82)
    val validatorPublic: Array[Byte] = Array (78, 40, 39, 28, 74, 65)
    val aliceToBob = SignedTransaction.pay (alicePublic, bobPublic, 100, alicePrivate)
    val bobToAlice = SignedTransaction.pay (bobPublic, alicePublic, 100, bobPrivate)
    val transactions = Array (aliceToBob, bobToAlice)

    val before = System.currentTimeMillis()
    val subject = Block (transactions, validatorPublic)
    val after = System.currentTimeMillis()

    it ("has the expected data in it") {
      assert (subject.transactions === transactions)
      assert (subject.timestamp >= before)
      assert (subject.timestamp <= after)
      assert (subject.validator === validatorPublic)
      assert (subject.nonce.isEmpty === true)
    }

    it ("can be made Pretty in a reasonable number of tries") {
      val limit = 1 << (Block.REQUIRED_ZEROS + 1) // twice the mean
      var value = 0
      var prettyFound = false
      while (!prettyFound && (value < limit)) {
        val nonce = Array (
          ((value >> 24) & 0xFF).asInstanceOf[Byte],
          ((value >> 16) & 0xFF).asInstanceOf[Byte],
          ((value >> 8) & 0xFF).asInstanceOf[Byte],
          ((value >> 0) & 0xFF).asInstanceOf[Byte]
        )
        val attempt = subject.withNonce (nonce)
        prettyFound = attempt.isPretty
        value += 1
      }
      assert (prettyFound === true)
    }
  }

  def makeKeyPair (bytes: Byte*): (Array[Byte], Array[Byte]) = {
    val privateKey = bytes.toArray
    val publicKey = Utils.translateKey(privateKey)
    (privateKey, publicKey)
  }
}
