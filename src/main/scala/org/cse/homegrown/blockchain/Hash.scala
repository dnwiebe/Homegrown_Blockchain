package org.cse.homegrown.blockchain

import org.cse.homegrown.utils.Utils

case class Hash (value: Array[Byte]) {
  override def equals (o: Any): Boolean = {
    if (o == null) false
    else if (!o.isInstanceOf[Hash]) false
    else {
      val that = o.asInstanceOf[Hash]
      this.value.sameElements (that.value)
    }
  }

  override def hashCode (): Int = {
    value.indices.foldLeft (0) {(soFar, idx) =>
      val byte = value (idx)
      val bitOffset = (idx & 0x3) << 3
      soFar + (byte << bitOffset)
    }
  }

  override def toString: String = {
    Utils.stringFromBytes (value)
  }
}

trait HashFactory {
  def apply (bytes: Seq[Byte]): Hash
}

object SHA_256 extends HashFactory {
  def apply (bytes: Seq[Byte]): Hash = {
    val digest = java.security.MessageDigest.getInstance ("SHA-256")
    bytes.foreach (digest.update)
    Hash (digest.digest ())
  }
}