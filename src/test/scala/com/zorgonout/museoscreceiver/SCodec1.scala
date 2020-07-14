package com.zorgonout.museoscreceiver

import org.scalatest.matchers.should
import org.scalatest.wordspec.AnyWordSpec
import scodec.Attempt.{Failure, Successful}
import scodec.{DecodeResult, Err, codecs}
import scodec.bits._
import scodec.codecs._

class SCodec1 extends AnyWordSpec with should.Matchers {
  "SCodec" should {

    "encode int8" in {
      val encoded = int8.encode(42)
      encoded shouldBe Successful(BitVector(42))
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

    "encode and decode int32 to BigDecimal" in {
      val bigDecimalCodec = int32.xmapc(BigDecimal(_))(_.intValue)
      val encoded = bigDecimalCodec.encode(BigDecimal(80.25))
      encoded shouldBe int32.encode(80)
      val decoded = bigDecimalCodec.decode(encoded.require)
      decoded shouldBe Successful(DecodeResult(80, BitVector.empty))
    }

    //    "decode discriminator" in {
    //      val x = discriminated
    //    }


  }


}
