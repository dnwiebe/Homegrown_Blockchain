package org.cse.homegrown.utils

import org.cse.homegrown.application.ceesy.{PrivateKey, PublicKey}

import scala.util.Random

object TestUtils {

  def makeKeyPair (bytes: Byte*): (PrivateKey, PublicKey) = {
    val privateKey = new PrivateKey (bytes.toArray)
    val publicKey = Utils.translateKey(privateKey)
    (privateKey, publicKey)
  }

  def makeKeyPair (seed: Int): (PrivateKey, PublicKey) = {
    val random = new Random(seed)
    val privateKey = new PrivateKey ((0 until 32).map (_ => random.nextInt ().asInstanceOf[Byte]).toArray)
    val publicKey = Utils.translateKey(privateKey)
    (privateKey, publicKey)
  }
}
