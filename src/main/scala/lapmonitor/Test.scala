package lapmonitor

import lapmonitor.UniqueId.{uniqueIdBytes, UniqueIdLength}

import java.lang
import java.math.BigInteger

/**
 * TODO find out how to actually start a race on the device.
 *   Examples below with min 5s lap times
 *
 * TODO find a bluetooth library
 *   for the jvm latest bluecove port https://gitlab.com/ultreiaio/bluecove
 *     or https://stackoverflow.com/questions/22892738/mac-os-and-java-bluetooth/41319931#41319931
 *   Or switch to rust https://github.com/deviceplug/btleplug
 *   Or switch to js https://www.npmjs.com/package/node-bluetooth
 */
object Test extends App {

  // same in hex: f0 25 bf 68
  private final val fixedBytes = Array[Byte](-16, 37, -65, 104)

  def hexToBytes(hexString: String) = new BigInteger(hexString, 16).toByteArray

  val testMessages = Seq(
    hexToBytes("236c01560004000300f1df00a5"),
    hexToBytes("236c0156000500030027d400a5"),
    hexToBytes("236c01560006000300b0c900a5"),
    hexToBytes("236c0156000700030063a700a5"),
    hexToBytes("236c01560008000300f69d00a5"),
    hexToBytes("237301034cb99e20dc0ac21d1c4a560019003a99")
  )

  testMessages.foreach { msg =>
    val parsed = parse(msg)
    println(parsed)
  }

  // to get these messages:
  //   connect the LapMonitor app to bluetooth device
  //   start race
  //   wait fot it to begin
  //   disconnect app
  //   connect mac Serial app to bluetooth device

  // data received when in a race (already started, transponder nearby always registering, 1 transponder, 5 sec min)
  // 5 seconds between lap time as that is the min duration set
  // ...
  // 236c01560004000300f1df00a5
  // 236c0156000500030027d400a5
  // 236c01560006000300b0c900a5
  // 236c0156000700030063a700a5
  // 236c01560008000300f69d00a5
  // 236c015600090003003c9200a5
  // 236c0156000a000300a78700a5
  // 236c0156000b000300777d00a5
  // 236c0156000c000300e3b300a5
  // 236c0156000d00030009a800a5
  // 236c0156000e000300929d00a5
  // 236c0156000f000300429200a5
  // 236c01560010000300c78700a5
  // 236c015600110003001d7c00a5
  // 236c01560012000300867100a5
  // 236c01560013000300566600a5
  // 236c01560014000300cb5b00a5
  // 236c01560015000300015100a5
  // 236c015600160003009a4700a5
  // 236c015600170003004a3c00a5
  // 236c01560018000300df3100a5
  // 236c01560019000300052600a5
  // 237301034 cb99e20dc0ac21d1c4a560019 003a99

  // other similar session
  // ...
  // 236c01c7001c000300d8da00a5
  // 236c01c7001d0003000ecf00a5
  // 236c01c7001e00030099c400a5
  // 237301034 2e29c232ba8c85b23e4c7001e 003a99

  // other similar session
  // ...
  // 236c0118001e00030099c300a5
  // 237301034 2bba619151960b3a8b018001e 003a99

  // byte 0 is always 0x23

  // byte 1 is the selector
  //   0x6c = lap event
  //   0x73 = status message

  def parse(message: Array[Byte]): Event = message(1) match {
    case 0x6c  => parseLapEvent(message)
    case 0x73  => parseStatusMessage(message)
    case other => throw new UnsupportedOperationException(s"Unknown message with selector: ${byteToHex(other)}")
  }

  def maybeStart = {
    val bytes = new Array[Byte](19)
    bytes(0) = 35.toByte // hex 23
    bytes(1) = 83.toByte // hex 53
    bytes(2) = 1.toByte  // hex 01 Is this the status?
    //bytes(3) = this.O.toByte // ??? hex 56 here
    bytes(4) = 0.toByte // hex 00

    // write unique id (race id?)
    val sessionIdBytes = uniqueIdBytes
    writeUUIDBytes(bytes, 5, sessionIdBytes)

    val a: Long = ???
    val d: Long = ???
    val e: Long = ???

    val l = a + d
    bytes(15) = (l >> 8L & 0xffL).toInt.toByte
    bytes(16) = (l & 0xffL).toInt.toByte
    bytes(17) = (e >> 8L & 0xffL).toInt.toByte
    bytes(18) = (e & 0xffL).toInt.toByte
    bytes
  }

  def parseStatusMessage(bytes: Array[Byte]) = {
    // byte 0 always 0x23
    // byte 1 selector always 0x73 for status
    // what is byte 2?

    val statusInt = bytes(3) & 0xff
    val status    = Status(statusInt)

    val receivedUUID = readUUIDBytes(bytes, 4)

    // what are these?
    val b14 = bytes(14)
    val b15 = bytes(15) // always 0x00?
    val b16 = bytes(16)
    val b17 = bytes(17) // always 0x00
    val b18 = bytes(18) // always 0x3a
    val b19 = bytes(19) // always 0x99
    //println(byteToHex(b17))

    StatusEvent(status, receivedUUID)
  }

  private def parseLapEvent(bytes: Array[Byte]) = {

    val b4        = bytes(4) // lap hi
    val b5        = bytes(5) // lap low
    val lapNumber = Integer.valueOf((b4 & 0xff) << 8 | b5 & 0xff)

    val b6            = bytes(6) // transponder hi
    val b7            = bytes(7) // transponder low
    val transponderId = Integer.valueOf((b6 & 0xff) << 8 | b7 & 0xff).intValue

    // three bytes for the time
    // TODO how does this decoding work?
    val b8  = bytes(8)
    val b9  = bytes(9)
    val b10 = bytes(10)
    val hmm = fixedBytes(lapNumber.intValue % fixedBytes.length)
    val i = Integer
      .valueOf(hmm & 0xff ^ b9 & 0xff ^ lapNumber.intValue & 0xff)
      .intValue
    val j                             = Integer.valueOf(i << 8).intValue
    val elapsedSinceStartHectoSeconds = Integer.valueOf(j | (b8 & lang.Byte.MAX_VALUE) << 16 | b10 & 0xff).intValue

    val b11 = bytes(11) // always 0x00
    val b12 = bytes(12) // always 0xa5
    LapEvent(
      transponderId,
      elapsedSinceStartHectoSeconds,
      lapNumber.intValue
    )
  }

  private def readUUIDBytes(bytes: Array[Byte], offset: Int): Array[Byte] = {
    val receivedUUID = new Array[Byte](UniqueIdLength)
    var index        = 0
    while (index < UniqueIdLength) {
      receivedUUID(index) = bytes(index + offset)
      index += 1
    }
    receivedUUID
  }

  private def writeUUIDBytes(bytes: Array[Byte], offset: Int, uniqueId: Array[Byte]): Unit = {
    var b1 = 0
    while (b1 < UniqueIdLength) {
      bytes(b1 + offset) = uniqueId(b1)
      b1 += 1
    }
  }

  private def byteToHex(b12: Byte) =
    String.format("%02x", b12)
}
