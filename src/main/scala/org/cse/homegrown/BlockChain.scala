package org.cse.homegrown

class BlockChain {
  private var chain = List (Block (0, System.currentTimeMillis(), "Genesis Block", Hash (Array ())))

  def latest: Block = {
    chain.head
  }

  def add (content: Any): Unit = {
    val previousBlock = latest
    val thisBlock = Block (previousBlock.index + 1, System.currentTimeMillis(), content, previousBlock.hash)
    chain = thisBlock :: chain
  }

  def isValid: Boolean = {
    !chain.zip (chain.tail).exists {pair =>
      val (later, earlier) = pair
      (SHA_256 (later.data) != later.hash) ||
        (later.previousHash != earlier.hash)
    }
  }
}
