package org.cse.homegrown.blockchain

import org.scalatest.path

class BlockChainTest extends path.FunSpec {

  describe ("A new BlockChain") {
    val before = System.currentTimeMillis()
    val subject = new BlockChain ("Genesis Block")
    val after = System.currentTimeMillis()

    describe ("asked for its latest block") {
      val result = subject.latest
      val block = new BlockWrapper (0, 0, "Genesis Block", new Hash (Array ()))

      it ("produces the genesis block") {
        assert (result.index === 0)
        assert (result.timestamp >= before)
        assert (result.timestamp <= after)
        assert (result.data === block.data)
        assert (result.hash === block.hash)
        assert (result.previousHash === new Hash (Array ()))
      }
    }

    describe ("asked if it's valid") {
      val result = subject.isValid

      it ("says yes") {
        assert (result === true)
      }
    }

    describe ("with a block added") {
      val genesisBlock = subject.latest
      val content = "Booga"
      val before = System.currentTimeMillis()
      subject.add (content)
      val after = System.currentTimeMillis()

      describe ("and then asked for its latest block") {
        val result = subject.latest
        val block = new BlockWrapper (0, 0, content, new Hash (Array ()))

        it ("produces the new block") {
          assert (result.index === 1)
          assert (result.timestamp >= before)
          assert (result.timestamp <= after)
          assert (result.content (classOf[String]) === content)
          assert (result.hash === block.hash)
          assert (result.previousHash === genesisBlock.hash)
        }
      }

      describe ("asked if it's valid") {
        val result = subject.isValid

        it ("says yes") {
          assert (result === true)
        }
      }
    }
  }
}
