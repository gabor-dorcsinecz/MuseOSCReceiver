package com.zorgonout.museoscreceiver

import org.scalatest.matchers.should
import org.scalatest.wordspec.AnyWordSpec
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
      val decodecAccount = accountCodec.decode(encodedAccount.require)
      decodecAccount.require.value shouldBe account
    }
  }
}
