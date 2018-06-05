package org.cse.homegrown.application.ceesy

import org.cse.homegrown.blockchain.ByteSeq

class Key (bytes: Array[Byte]) extends ByteSeq (bytes)
class PublicKey (bytes: Array[Byte]) extends Key (bytes)
class PrivateKey (bytes: Array[Byte]) extends Key (bytes)
class Nonce (bytes: Array[Byte]) extends ByteSeq (bytes)
class PlainData (bytes: Array[Byte]) extends ByteSeq (bytes)
class CryptData (bytes: Array[Byte]) extends ByteSeq (bytes)
