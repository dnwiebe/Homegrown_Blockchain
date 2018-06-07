package org.cse.homegrown.utils

trait Timestamper extends Serializable {
  def stamp (): Long
}

class RealTimestamper extends Timestamper {
  override def stamp (): Long = System.currentTimeMillis()
}
