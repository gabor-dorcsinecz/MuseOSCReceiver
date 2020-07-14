package com.zorgonout.museoscreceiver

import org.scalatest.matchers._
import org.scalatest.wordspec.AnyWordSpec
import  com.zorgonout.museoscreceiver.OSC

class OSCParserSpec extends AnyWordSpec with should.Matchers {
  "it" should {
    "Parse one message" in {
      val oneChunk = MuseIncomingOSC.example1(0)
      //println(oneChunk.map(a => a.toHexString))
      //println(oneChunk.map(a => a.toChar))
      //println(MuseIncomingOSC.example1.map(a => a.map(b => b.toChar)).mkString("\r\n"))
      val decoded = OSC.packageCodec.decode(oneChunk.toBitVector)
      //println(decoded)
      decoded.isSuccessful shouldBe true
    }
  }
}
