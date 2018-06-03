package org.cse.homegrown

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
}
