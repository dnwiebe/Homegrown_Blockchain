package org.cse.homegrown.utils

import org.scalatest.path

@SerialVersionUID (1L)
case class Person (first: String, last: String, age: Int) extends Serializable

class UtilsTest extends path.FunSpec {

  describe ("An array of bytes") {
    val data: Array[Byte] = Array (12, 34, 56, 78, 90)

    describe ("converted into a String") {
      val result = Utils.stringFromBytes (data)

      it ("produces the expected result") {
        assert (result === "0C22384E5A")
      }
    }
  }

  describe ("A Person, serialized and encrypted") {
    val data = Person ("Dave", "Hardiman", 34)
    val privateKey: Array[Byte] = Array (90, 23, 80, 30, 20, 93, 47, 2)
    val publicKey = Utils.translateKey (privateKey)
    val serialized = Utils.serialize (data)
    val encrypted = Utils.encrypt (serialized, privateKey)

    describe ("and then decrypted and deserialized") {
      val decrypted = Utils.decrypt (encrypted, publicKey)
      val result = Utils.deserialize (decrypted, classOf[Person])

      it ("produces the original data") {
        assert (result === data)
      }
    }
  }
}
