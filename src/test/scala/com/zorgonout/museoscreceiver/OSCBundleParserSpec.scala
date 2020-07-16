package com.zorgonout.museoscreceiver

import org.scalatest.matchers._
import org.scalatest.wordspec.AnyWordSpec
import com.zorgonout.museoscreceiver.OSC
import com.zorgonout.museoscreceiver.OSC.{OSCBundle, OSCFloat, OSCMessage}

class OSCBundleParserSpec extends AnyWordSpec with should.Matchers {
  "it" should {
    "Parse one message" in {
      val chunk = MuseIncomingOSC.example1(0)
      //println(chunk.map(a => a.toHexString))
      //println(chunk.map(a => a.toChar))
      val decoded = OSC.packageCodec.decode(chunk.toBitVector)
      //println(decoded)
      decoded.isSuccessful shouldBe true
      decoded.require.value.getClass shouldBe classOf[OSCBundle]
      val bundle = decoded.require.value.asInstanceOf[OSCBundle]
      bundle.bundleName.init shouldBe "bundle"
      bundle.timeTag.toString shouldBe "2020-07-10T17:46:40.240518168"
      bundle.data.getClass shouldBe classOf[OSCMessage]
      bundle.data.address shouldBe "/muse/eeg"
      bundle.data.data shouldBe List(OSCFloat(855.3898F), OSCFloat(848.8099F), OSCFloat(837.295F), OSCFloat(827.4251F))
    }
  }

  "Parse all messages" in {
    MuseIncomingOSC.example1.foreach{ chunk =>
      val decoded = OSC.packageCodec.decode(chunk.toBitVector)
      decoded.isSuccessful shouldBe true
      decoded.require.value.getClass shouldBe classOf[OSCBundle]
      val bundle = decoded.require.value.asInstanceOf[OSCBundle]
      bundle.bundleName.init shouldBe "bundle"
    }
  }
}
