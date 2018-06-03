package org.cse.homegrown

import org.cse.homegrown.utils.{Person, Utils}
import org.scalatest.path

class TransactionTest extends path.FunSpec {

  describe ("Given a couple of different objects and a key pair") {
    val onePerson = Person ("Simon", "Cowell", 68)
    val anotherPerson = Person ("Howie", "Mandel", 62)
    val privateKey: Array[Byte] = Array (34, 52, 34, 52, 35, 63, 4)
    val publicKey = Utils.translateKey (privateKey)

    describe ("a signature of one of the items with the private key") {
      val subject = Signature.sign (onePerson, privateKey)

      it ("validates with that item and the public key") {
        assert (subject.verify (onePerson, publicKey) === true)
      }

      it ("does not validate with a different item and the public key") {
        assert (subject.verify (anotherPerson, publicKey) === false)
      }

      it ("does not validate with that item and a different key") {
        assert (subject.verify (onePerson, privateKey) === false)
      }
    }
  }

  describe ("Given a key pair and an extra public key") {
    val fromPrivate: Array[Byte] = Array (34, 52, 34, 52, 35, 63, 4)
    val fromPublic = Utils.translateKey (fromPrivate)
    val toPublic: Array[Byte] = Array (98, 34, 72, 9, 35, 7, 92)

    describe ("a SignedTransaction from one to the other") {
      val before = System.currentTimeMillis ()
      val subject = SignedTransaction.pay (fromPublic, toPublic, 12345, fromPrivate)
      val after = System.currentTimeMillis ()

      it ("shows the right data") {
        assert (subject.timestamp >= before)
        assert (subject.timestamp <= after)
        assert (subject.from.sameElements (fromPublic))
        assert (subject.to.sameElements (toPublic))
        assert (subject.amount === 12345)
      }

      it ("validates fine") {
        assert (subject.verify () === true)
      }
    }

    describe ("a cobbled-together SignedTransaction") {
      val subject = new SignedTransaction (new Transaction (fromPublic, toPublic, 12345, 0L), Signature (Array (1, 2, 3)))

      it ("does not validate") {
        assert (subject.verify () === false)
      }
    }
  }
}
