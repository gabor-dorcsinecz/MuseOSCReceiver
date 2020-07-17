package com.zorgonout.museoscreceiver

import com.zorgonout.museoscreceiver.OSC.{OSCBundle, OSCFloat, OSCMessage}
import fs2.Chunk
import org.scalatest.matchers._
import org.scalatest.wordspec.AnyWordSpec
import scodec.bits._

class OSCBundleParserSpec extends AnyWordSpec with should.Matchers {
  "it" should {
    "Parse one message" in {
      val chunk = MuseIncomingOSC.example1(0)
      //println(chunk.map(a => a.toHexString))
      //println(chunk.map(a => a.toChar))
      val decoded = OSC.oscPackageCodec.decode(chunk.toBitVector)
      //println(decoded)
      decoded.isSuccessful shouldBe true
      decoded.require.value.getClass shouldBe classOf[OSCBundle]
      val bundle = decoded.require.value.asInstanceOf[OSCBundle]
      bundle.bundleName.init shouldBe "bundle"
      bundle.timeTag.toString shouldBe "2020-07-10T17:46:40.055999968"
      bundle.data.getClass shouldBe classOf[OSCMessage]
      bundle.data.address shouldBe "/muse/eeg"
      bundle.data.data shouldBe List(OSCFloat(855.3898F), OSCFloat(848.8099F), OSCFloat(837.295F), OSCFloat(827.4251F))
    }
  }

  "Parse invalid nanosecond" in {
    val packet = Chunk.byteVector(hex"2362756e646c6500e2badf3c3eb851eb000000242f6d7573652f6565670000002c666666660000004447dd7844522570445225704442ee20")
    val decoded = OSC.oscPackageCodec.decode(packet.toBitVector)
  }

  "Parse all messages" in {
    MuseIncomingOSC.example1.foreach{ chunk =>
      val decoded = OSC.oscPackageCodec.decode(chunk.toBitVector)
      decoded.isSuccessful shouldBe true
      decoded.require.value.getClass shouldBe classOf[OSCBundle]
      val bundle = decoded.require.value.asInstanceOf[OSCBundle]
      bundle.bundleName.init shouldBe "bundle"
    }
  }
}
