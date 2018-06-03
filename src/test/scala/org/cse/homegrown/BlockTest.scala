package org.cse.homegrown

import org.cse.homegrown.utils.Utils
import org.scalatest.path

@SerialVersionUID (1L)
case class Person (first: String, last: String, age: Int) extends Serializable

class BlockTest extends path.FunSpec {

  describe ("A Block") {
    val content = new Person ("Fred", "Milligan", 49)
    val subject = Block (1L, 2L, content, Hash (Array (3, 4, 5)))

    it ("has the expected index") {
      assert (subject.index === 1L)
    }

    it ("has the expected timestamp") {
      assert (subject.timestamp === 2L)
    }

    it ("has the expected data") {
      assert (subject.content (classOf[Person]) === content)
    }

    it ("has the expected previous-block hash") {
      assert (subject.previousHash.value === Hash (Array (3, 4, 5)).value)
    }

    describe ("with its hash converted to a string") {
      val string = subject.hash.value.map (Utils.stringFromByte).mkString ("")

      it("has the expected value") {
        assert(string === "554699442780B3EBD7355356FE88B5867700778E6B5F0D363E46C2DF719F5131")
      }
    }
  }
}
