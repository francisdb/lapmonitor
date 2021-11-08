package lapmonitor

import java.nio.ByteBuffer
import java.util.UUID

object UniqueId {
  final val UniqueIdLength = 10

  private final val FourBitChars: Array[Char] = "0123456789ABCDEF".toCharArray

  // TODO do we need this per session we start?
  final lazy val uniqueIdBytes: Array[Byte] = {
    val uUID        = UUID.randomUUID
    val l1          = uUID.getMostSignificantBits
    val l2          = uUID.getLeastSignificantBits
    val arrayOfByte = ByteBuffer.allocate(16).putLong(l1).putLong(l2).array
    val c           = new Array[Byte](UniqueIdLength)
    var b1          = 0
    while (b1 < UniqueIdLength) {
      c(b1) = arrayOfByte(b1 + 6)
      b1 += 1
    }
    c
  }

  def uniqueIdToString(bytes: Array[Byte]): String = {
    assert(bytes.length == UniqueIdLength, "We expect 10 bytes for the unique id")
    val arrayOfChar = new Array[Char](bytes.length * 2)
    var b1          = 0
    while (b1 < bytes.length) {
      val i = bytes(b1) & 0xff
      val j = b1 * 2
      arrayOfChar(j) = FourBitChars(i >>> 4)
      arrayOfChar(j + 1) = FourBitChars(i & 0xf)
      b1 += 1
    }
    new String(arrayOfChar)
  }
}
