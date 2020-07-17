package com.zorgonout.museoscreceiver

import org.scalatest.matchers.should
import org.scalatest.wordspec.AnyWordSpec
import scodec.Attempt.{Failure, Successful}
import scodec.{DecodeResult, Err, codecs}
import scodec.bits._
import scodec.codecs._

class SCodecBasicsSpec extends AnyWordSpec with should.Matchers {
  "SCodec" should {

    "encode int8" in {
      val encoded = int8.encode(42)  //Successful(BitVector(8 bits, 0x2a))
      encoded shouldBe Successful(BitVector(42))
    }

    "encode int8 > 127" in {
      val encoded = int8.encode(200)  //Successful(BitVector(8 bits, 0x2a))
      encoded shouldBe Failure(Err("200 is greater than maximum value 127 for 8-bit signed integer"))
    }

    "encode uint8 > 127" in {
      val encoded = uint8.encode(200)  //Successful(BitVector(8 bits, 0xc8))
      encoded shouldBe Successful(BitVector(200))
    }


    "fail encoding int8 with big numbers" in {
      val encoded = int8.encode(420)
      encoded shouldBe Failure(Err("420 is greater than maximum value 127 for 8-bit signed integer"))
    }

    "encode utf8" in {
      val encoded = utf8.encode("Joe") //Successful(BitVector(24 bits, 0x4a6f65))
      encoded shouldBe Successful(BitVector("Joe".getBytes("UTF-8")))
    }

    "encode utf8 to bytes" in {
      val encoded = utf8.encode("Joe").require.bytes //ByteVector(3 bytes, 0x4a6f65)
      encoded shouldBe ByteVector("Joe".getBytes("UTF-8"))
    }

    "decode strings with fixedSizeBytes" in {
      val fsb = fixedSizeBytes(3, utf8) //  Codec[String]
      val decoded = fsb.decode(hex"4a6f65".bits)
      decoded shouldBe Successful(DecodeResult("Joe", BitVector.empty))
    }

    "decode string with variableSizeBytes" in {
      val vsb = variableSizeBytes(int8,utf8)
      val encoded = vsb.encode("Joe")   //Successful(BitVector(32 bits, 0x034a6f65))  Here we have 0x03 extra, to store the length
      val decoded = vsb.decode(encoded.require)
      decoded.require.value shouldBe "Joe"
    }

    "encode and decode int32 to BigDecimal" in {
      val bigDecimalCodec = int32.xmapc(BigDecimal(_))(_.intValue)
      val encoded = bigDecimalCodec.encode(BigDecimal(80.25))
      encoded shouldBe int32.encode(80)
      val decoded = bigDecimalCodec.decode(encoded.require)
      decoded shouldBe Successful(DecodeResult(80, BitVector.empty))
    }

  }


}
