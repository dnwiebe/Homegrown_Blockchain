package org.cse.homegrown.blockchain

import org.scalatest.path

class BlockChainTest extends path.FunSpec {

  describe ("A new BlockChain") {
    val hashedBlock = new BlockWrapper (0, 0, "Genesis Block", new Hash (Array ()))
    val before = System.currentTimeMillis()
    val subject = new BlockChain ("Genesis Block")
    val after = System.currentTimeMillis()

    describe ("asked for its latest block") {
      val result = subject.latest

      it ("produces the genesis block") {
        assert (result.index === 0)
        assert (result.timestamp >= before)
        assert (result.timestamp <= after)
        assert (result.data === hashedBlock.data)
        assert (result.hash === hashedBlock.hash)
        assert (result.previousHash === new Hash (Array ()))
      }
    }

    val genesisBlock = subject.latest

    describe ("asked for its leaf blocks") {
      val result = subject.leaves ()

      it ("produces only the genesis block") {
        assert (result === Set (genesisBlock))
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
        val block = BlockWrapper (0, 0, content, new Hash (Array ()))

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

    describe ("with two leaf blocks added to the genesis block") {
      val genesisBlock = subject.latest
      val someHash = subject.add ("some content", Some (genesisBlock.hash))
      val moreHash = subject.add ("more content", Some (genesisBlock.hash))

      describe ("and then asked for its leaf blocks") {
        val result = subject.leaves ()

        it ("produces the two leaves") {
          assert (result.map (x => x.content (classOf[String])) === Set ("some content", "more content"))
        }
      }

      describe ("and a third leaf block added") {
        val supercessiveHash = subject.add ("supercessive content", Some (moreHash))

        describe ("and the asked for its leaf blocks" ) {
          val result = subject.leaves (1000000L)

          it ("produces the third leaf but not its parent") {
            assert (result.map (x => x.content (classOf[String])) === Set ("some content", "supercessive content"))
          }
        }
      }
    }
  }
}
