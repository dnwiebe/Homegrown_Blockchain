package org.cse.homegrown.blockchain

class Hash (bytes: Array[Byte]) extends ByteSeq (bytes)

trait HashFactory {
  def apply (data: ByteSeq): Hash
}

object SHA_256 extends HashFactory {
  def apply (data: ByteSeq): Hash = {
    val digest = java.security.MessageDigest.getInstance ("SHA-256")
    data.bytes.foreach (digest.update)
    new Hash (digest.digest ())
  }
}
