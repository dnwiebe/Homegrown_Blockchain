package org.cse.homegrown.blockchain

import org.cse.homegrown.utils.{RealTimestamper, Timestamper}

import scala.collection.mutable

class BlockChain (genesisBlockContent: Any) {
  var timestamper: Timestamper = new RealTimestamper ()
  private val genesisBlock = BlockWrapper (0, timestamper.stamp (), genesisBlockContent, new Hash (Array ()))
  private var latestHash = genesisBlock.hash
  private var leafHashes: Set[Hash] = Set (genesisBlock.hash)
  private val chain = mutable.HashMap (latestHash -> genesisBlock)

  def latest: BlockWrapper = {
    chain (latestHash)
  }

  def leaves (intervalMs: Long = 86400000L): Set[BlockWrapper] = leafHashes.flatMap (chain.get)

  def block (hash: Hash): Option[BlockWrapper] = {
    chain.get (hash)
  }

  def add (content: Any, previousHashOpt: Option[Hash] = None): Hash = {
    val previousHash = previousHashOpt.getOrElse (latestHash)
    val previousBlock = block (previousHash).get
    val thisBlock = BlockWrapper (previousBlock.index + 1, timestamper.stamp (), content, previousBlock.hash)
    chain (thisBlock.hash) = thisBlock
    latestHash = thisBlock.hash
    curateLeaves (thisBlock)
    thisBlock.hash
  }

  def isValid: Boolean = {
    isValidFrom(latest)
  }

  private def isValidFrom (blockWrapper: BlockWrapper): Boolean = {
    if (blockWrapper.previousHash.bytes.isEmpty) return true
    block (blockWrapper.previousHash) match {
      case None => false
      case Some (previous) => {
        (SHA_256 (blockWrapper.data) == blockWrapper.hash) &&
          (blockWrapper.previousHash == previous.hash) &&
        isValidFrom (previous)
      }
    }
  }

  private def curateLeaves (newBlock: BlockWrapper): Unit = {
    leafHashes -= newBlock.previousHash
    leafHashes += newBlock.hash
  }
}
