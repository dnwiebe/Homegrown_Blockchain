package org.cse.homegrown.blockchain

import org.cse.homegrown.utils.Utils
import org.scalatest.path

class HashTest extends path.FunSpec {

  describe ("A SHA-256 hash of a string") {
    val subject = SHA_256 ("These are the times that try men's souls".getBytes ())

    describe ("converted to a string") {
      val string = subject.value.map (Utils.stringFromByte).mkString ("")

      it("has the expected value") {
        assert(string === "9EAEBC753F1400B6A797990D13615D2AD7D7793BB1C1418EF7201DC743EBA7A4")
      }
    }
  }

  describe ("A bunch of Hashes for testing equality") {
    val a = Hash (Array (1, 2, 3, 4))
    val b = Hash (Array (1, 2, 3, 4))
    val c = Hash (Array (1, 2, 3, 5))

    it ("produce the expected results") {
      assert (a.equals (a) === true)
      assert (a.equals (b) === true)
      assert (a.equals (c) === false)
      assert (a.equals (null) === false)
      assert (a.equals ("Booga") === false)
    }
  }

  describe ("A bunch of Hashes for testing hashCode") {
    val a = Hash (Array (1, 2, 3, 4))
    val b = Hash (Array (1, 2, 3, 4))
    val c = Hash (Array (1, 2, 3, 5))

    it ("produce the expected results") {
      assert (a.hashCode () === a.hashCode ())
      assert (a.hashCode () === b.hashCode ())
      assert (a.hashCode () != c.hashCode ())
    }
  }
}
