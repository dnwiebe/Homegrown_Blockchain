package org.cse.homegrown.utils


class ByteSeq (val bytes: Array[Byte]) extends Serializable {

  override def toString: String = Utils.stringFromBytes (bytes)

  override def equals (o: Any): Boolean = {
    if (o == null) false
    else if (o.getClass != this.getClass) false
    else o.asInstanceOf[ByteSeq].bytes.sameElements (this.bytes)
  }

  override def hashCode: Int = {
    bytes.indices.foldLeft (0) {(soFar, idx) =>
      val byte = bytes (idx)
      val bitOffset = (idx & 0x3) << 3
      soFar + (byte << bitOffset)
    }
  }
}
