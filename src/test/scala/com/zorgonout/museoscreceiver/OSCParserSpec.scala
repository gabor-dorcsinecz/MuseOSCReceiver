package com.zorgonout.museoscreceiver

import org.scalatest.matchers._
import org.scalatest.wordspec.AnyWordSpec
import com.zorgonout.museoscreceiver.OSC
import com.zorgonout.museoscreceiver.OSC.{OSCBundle, OSCMessage}

class OSCParserSpec extends AnyWordSpec with should.Matchers {
  "it" should {
    "Parse one message" in {
      val oneChunk = MuseIncomingOSC.example1(0)
      println(oneChunk.map(a => a.toHexString))
      println(oneChunk.map(a => a.toChar))
      //println(MuseIncomingOSC.example1.map(a => a.map(b => b.toChar)).mkString("\r\n"))
      val decoded = OSC.packageCodec.decode(oneChunk.toBitVector)
      println(decoded)
      decoded.isSuccessful shouldBe true
      decoded.require.value.getClass shouldBe classOf[OSCBundle]
      val bundle = decoded.require.value.asInstanceOf[OSCBundle]
      bundle.bundleName.init shouldBe "bundle"
      bundle.timeTag.toString shouldBe "2020-07-10T17:46:40.240518168"
      //bundle.data.getClass shouldBe classOf[OSCMessage]
    }
  }
}
