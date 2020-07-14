package com.zorgonout.museoscreceiver

import org.scalatest.matchers.should
import org.scalatest.wordspec.AnyWordSpec
import scodec.{Codec, DecodeResult}
import scodec.codecs._

class SCodec2 extends AnyWordSpec with should.Matchers {
  "case classes" should {
    "be encoded and decoded" in {
      case class Account(accountNumber:Int, name:String, balance:BigDecimal)
      val bigDecimalCodec = int32.xmapc(BigDecimal(_))(_.intValue)
      val stringCodec = variableSizeBytes(int32, utf8)
      //val accountCodec = (int32 :: utf8 :: bigDecimalCodec).as[Account]  //This will fail because utf8 is greedy, and will gobble up all the bytes
      val accountCodec = (int32 :: stringCodec :: bigDecimalCodec).as[Account]
      val account = Account(1000, "Pista", BigDecimal(2000))
      val encodedAccount = accountCodec.encode(account)
      println(encodedAccount.require.bytes.toSeq.map(a => a.toHexString))
      println(encodedAccount.require.bytes.toSeq.map(a => a.toChar))
      val decodecAccount = accountCodec.decode(encodedAccount.require)
      decodecAccount.require.value shouldBe account
    }
  }

  "discriminated" should {
    "discriminate based on Enumeration" in {
      object VersionType extends Enumeration {
        val MSG = Value(47,"/")
        val BUNDLE = Value(35,"#")
      }

      sealed trait OCSPackt
      case class Message(name:String) extends OCSPackt
      case class Bundle(name:String) extends OCSPackt

      val versionTypeCodec = enumerated(uint8, VersionType)

      val versionCodec = discriminated[OCSPackt]
        .by(versionTypeCodec)
        .|(VersionType.MSG) {case m:Message => m.name} (a => Message(a))(utf8)
        .|(VersionType.BUNDLE) {case b:Bundle => b.name} (a => Bundle(a))(utf8)

      val message = Message("Message")
      val encoded = versionCodec.encode(message)   //Successful(BitVector(64 bits, 0x2f4d657373616765))
      val printed = encoded.require.bytes.toSeq.map(a => a.toChar).mkString("")  //   "/Message"
      printed shouldBe "/Message"
      val decoded = versionCodec.decode(encoded.require)  //Successful(DecodeResult(Message(Message),BitVector(empty)))
      decoded.require.value shouldBe  message
    }
  }
}
