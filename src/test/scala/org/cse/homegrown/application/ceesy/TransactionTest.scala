package org.cse.homegrown.application.ceesy

import org.cse.homegrown.utils.TestUtils.OffsetTimestamper
import org.cse.homegrown.utils._
import org.scalatest.path

import scala.concurrent.Promise
import scala.util.Success

class TransactionTest extends path.FunSpec {

  describe ("Given a couple of different objects and a key pair") {
    val onePerson = Person ("Simon", "Cowell", 68)
    val anotherPerson = Person ("Howie", "Mandel", 62)
    val (privateKey, publicKey) = TestUtils.makeKeyPair(12345)

    describe ("a signature of one of the items with the private key") {
      val subject = Signature.sign (onePerson, privateKey)

      it ("validates with that item and the public key") {
        assert (subject.verify (onePerson, publicKey) === true)
      }

      it ("does not validate with a different item and the public key") {
        assert (subject.verify (anotherPerson, publicKey) === false)
      }

      it ("does not validate with that item and a different key") {
        assert (subject.verify (onePerson, TestUtils.makeKeyPair (23456)._2) === false)
      }
    }
  }

  describe ("Given a key pair and an extra public key") {
    implicit val timestamper: Timestamper = new OffsetTimestamper ()
    val (fromPrivate, fromPublic) = TestUtils.makeKeyPair (12345)
    val (_, toPublic) = TestUtils.makeKeyPair (23456)

    describe ("a SignedTransaction from one to the other") {
      val before = timestamper.stamp ()
      val (subject, verifyFuture) = SignedTransaction.pay (fromPublic, toPublic, 12345, fromPrivate)
      val after = timestamper.stamp ()

      it ("shows the right data") {
        assert (subject.timestamp >= before)
        assert (subject.timestamp <= after)
        assert (subject.from === fromPublic)
        assert (subject.to === toPublic)
        assert (subject.amount === 12345)
        assert (verifyFuture.isCompleted === false)
      }

      describe ("verified") {
        val result = subject.verifySignature ()

        it ("verifies fine") {
          assert (result === true)
        }
      }
    }

    describe ("a cobbled-together SignedTransaction") {
      val verifyPromise = Promise[Boolean] ()
      val subject = new SignedTransaction (TransactionGuts (fromPublic, toPublic, 12345, 0L), new Signature (new ByteSeq (Array (1, 2, 3))), verifyPromise)
      val verifyFuture = verifyPromise.future

      describe ("verified") {
        val result = subject.verifySignature ()

        it ("does not verify") {
          assert (result === false)
        }
      }
    }
  }
}
