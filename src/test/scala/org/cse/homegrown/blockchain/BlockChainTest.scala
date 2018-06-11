package org.cse.homegrown.blockchain

import org.cse.homegrown.utils.TestUtils.OffsetTimestamper
import org.cse.homegrown.utils.Utils
import org.scalatest.path

class BlockChainTest extends path.FunSpec {

  describe ("A new BlockChain") {
    val timestamper = new OffsetTimestamper ()
    val before = timestamper.stamp ()
    val subject = new BlockChain ("Genesis Block")
    subject.timestamper = timestamper
    val after = timestamper.stamp ()

    describe ("asked for its latest block") {
      val result = subject.latest

      it ("produces the genesis block") {
        assert (result.timestamp >= before)
        assert (result.timestamp <= after)
        assert (result.data === Utils.serialize ("Genesis Block"))
        assert (result.hash === SHA_256 (Utils.serialize ("Genesis Block")))
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
      val before = timestamper.stamp ()
      subject.add (content)
      val after = timestamper.stamp ()

      describe ("and then asked for its latest block") {
        val result = subject.latest
        val block = BlockWrapper (0L, content, new Hash (Array ()))

        it ("produces the new block") {
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

    describe ("with three leaf blocks added to the genesis block, one old") {
      val genesisBlock = subject.latest
      val oldHash = subject.add ("old content", Some (genesisBlock.hash))
      timestamper.setOffset (100000000L)
      val someHash = subject.add ("some content", Some (genesisBlock.hash))
      val moreHash = subject.add ("more content", Some (genesisBlock.hash))

      describe ("and then asked for its leaf blocks") {
        val result = subject.leaves ()

        it ("produces the two new leaves") {
          assert (result.map (x => x.content (classOf[String])) === Set ("some content", "more content"))
        }
      }

      describe ("and a fourth leaf block added") {
        val supercessiveHash = subject.add ("supercessive content", Some (moreHash))

        describe ("and the asked for its leaf blocks" ) {
          val result = subject.leaves (1000000L)

          it ("produces the fourth leaf but not its parent") {
            assert (result.map (x => x.content (classOf[String])) === Set ("some content", "supercessive content"))
          }
        }
      }
    }
  }
}
