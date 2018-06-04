package org.cse.homegrown.utils

import scala.util.Random

object TestUtils {

  def makeKeyPair (bytes: Byte*): (Array[Byte], Array[Byte]) = {
    val privateKey = bytes.toArray
    val publicKey = Utils.translateKey(privateKey)
    (privateKey, publicKey)
  }

  def makeKeyPair (seed: Int): (Array[Byte], Array[Byte]) = {
    val random = new Random(seed)
    val privateKey = (0 until 32).map (_ => random.nextInt ().asInstanceOf[Byte]).toArray
    val publicKey = Utils.translateKey(privateKey)
    (privateKey, publicKey)
  }
}
