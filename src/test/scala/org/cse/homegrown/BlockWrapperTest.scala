package org.cse.homegrown

import org.cse.homegrown.utils.{Person, Utils}
import org.scalatest.path

class BlockWrapperTest extends path.FunSpec {

  describe ("A Block") {
    val content = Person ("Fred", "Milligan", 49)
    val subject = BlockWrapper (1L, 2L, content, Hash (Array (3, 4, 5)))

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
        assert(string === "2927CD5A379E796C0780D02842CF833BCD7E933531205946976E7EE3A71EBB95")
      }
    }
  }
}
