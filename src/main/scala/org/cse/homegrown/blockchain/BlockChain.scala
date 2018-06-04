package org.cse.homegrown.blockchain

import org.cse.homegrown.utils.Utils

import scala.collection.mutable

class BlockChain (genesisBlock: Any) {
  private var latestHash = SHA_256 (Utils.serialize (genesisBlock))
  private val chain = mutable.HashMap (latestHash -> BlockWrapper (0, System.currentTimeMillis (), genesisBlock, Hash (Array ())))

  def latest: BlockWrapper = {
    chain (latestHash)
  }

  def block (hash: Hash): Option[BlockWrapper] = {
    chain.get (hash)
  }

  def add (content: Any): Unit = {
    val previousBlock = latest
    val thisBlock = BlockWrapper (previousBlock.index + 1, System.currentTimeMillis(), content, previousBlock.hash)
    chain (thisBlock.hash) = thisBlock
    latestHash = thisBlock.hash
  }

  def isValid: Boolean = {
    isValidFrom(latest)
  }

  private def isValidFrom (blockWrapper: BlockWrapper): Boolean = {
    if (blockWrapper.previousHash.value.isEmpty) return true
    block (blockWrapper.previousHash) match {
      case None => false
      case Some (previous) => {
        (SHA_256 (blockWrapper.data) == blockWrapper.hash) &&
          (blockWrapper.previousHash == previous.hash) &&
        isValidFrom (previous)
      }
    }
  }
}
