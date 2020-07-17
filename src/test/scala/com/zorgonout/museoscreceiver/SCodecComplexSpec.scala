package com.zorgonout.museoscreceiver

import com.zorgonout.museoscreceiver.OSC.OSCFloat
import fs2.Chunk
import org.scalatest.matchers.should
import org.scalatest.wordspec.AnyWordSpec
import scodec.Attempt.Successful
import scodec.bits.{BitVector, ByteVector}
import scodec.{Attempt, Codec, DecodeResult, Encoder, Err, SizeBound}
import scodec.codecs._
import scodec.bits._

class SCodecComplexSpec extends AnyWordSpec with should.Matchers {

  "Codec creation" should {
    "be able to combine codecs" in {
      val x: Codec[(Int, ByteVector)] = uint8 flatZip { numBytes => bytes(numBytes) }
      val variableLength: Codec[ByteVector] = x.xmap[ByteVector]({ case (_, bv) => bv }, bv => (bv.size.toInt, bv))
      val encoded = variableLength.encode(ByteVector("Amina".getBytes("ASCII")))
      encoded.require.bytes.length shouldBe 6 //Because of the size added at the front
    }
  }

  "case classes" should {
    "be encoded and decoded" in {
      case class Account(accountNumber: Int, name: String, balance: BigDecimal)
      val bigDecimalCodec = int32.xmapc(BigDecimal(_))(_.intValue)
      val stringCodec = variableSizeBytes(int32, utf8)
      //val accountCodec = (int32 :: utf8 :: bigDecimalCodec).as[Account]  //This will fail because utf8 is greedy, and will gobble up all the bytes
      val accountCodec = (int32 :: stringCodec :: bigDecimalCodec).as[Account]
      val account = Account(1000, "Pista", BigDecimal(2000))
      val encodedAccount = accountCodec.encode(account)
      //println(encodedAccount.require.bytes.toSeq.map(a => a.toHexString))
      //println(encodedAccount.require.bytes.toSeq.map(a => a.toChar))
      val decodecAccount = accountCodec.decode(encodedAccount.require)
      decodecAccount.require.value shouldBe account
    }
  }

  "discriminated" should {
    "discriminate based on Enumeration" in {
      object VersionType extends Enumeration {
        val MSG = Value(47, "/")
        val BUNDLE = Value(35, "#")
      }

      sealed trait OCSPackt
      case class Message(name: String) extends OCSPackt
      case class Bundle(name: String) extends OCSPackt

      val versionTypeCodec = enumerated(uint8, VersionType)

      val versionCodec = discriminated[OCSPackt]
        .by(versionTypeCodec)
        .|(VersionType.MSG) { case m: Message => m.name }(a => Message(a))(utf8)
        .|(VersionType.BUNDLE) { case b: Bundle => b.name }(a => Bundle(a))(utf8)

      val message = Message("Message")
      val encoded = versionCodec.encode(message) //Successful(BitVector(64 bits, 0x2f4d657373616765))
      val printed = encoded.require.bytes.toSeq.map(a => a.toChar).mkString("") //   "/Message"
      printed shouldBe "/Message"
      val decoded = versionCodec.decode(encoded.require) //Successful(DecodeResult(Message(Message),BitVector(empty)))
      decoded.require.value shouldBe message
    }
  }

  "Codec Creation" should {
    "work" in {
      class KeepingCodec(keep: Long) extends Codec[BitVector] {
        override def sizeBound: SizeBound = SizeBound.unknown

        override def encode(buffer: BitVector): Attempt[BitVector] =
          Attempt.successful(buffer)

        override def decode(buffer: BitVector): Attempt[DecodeResult[BitVector]] =
          if (buffer.sizeGreaterThanOrEqual(keep)) {
            val (res, remaining) = buffer.splitAt(buffer.size - keep)
            Attempt.successful(DecodeResult(res, remaining))
          } else {
            Attempt.failure(Err.insufficientBits(keep, buffer.size))
          }
      }

      val kc = new KeepingCodec(7)
      val aminaBV = BitVector("Amina".getBytes("UTF-8"))
      val encoded = kc.encode(aminaBV)
      //println(encoded.require.bytes)
      val decoded = kc.decode(encoded.require)
      //println(decoded.require.value.bytes)
      //println(new String(decoded.require.value.bytes))
      //TODO What's happening here, and why does this not work???

      //This is supposed to do the same
      val bitsCodec = bytes(3)
      val encoded2 = bitsCodec.encode(aminaBV.bytes)
      //println(encoded2)
      val decoded2 = bitsCodec.decode(encoded.require)
      //println(decoded2)
    }
  }

  "finder codec" should {
    "find commas" in {
      val encodedData = Chunk.byteVector(hex"2f6d7573652f6565670000002c666666660000004455d8f2445433d5445152e1444edb35")
      val fc = new FinderCodec(',')
      val decoded = fc.decode(encodedData.toBitVector)
      decoded.require.value.trim() shouldBe "/muse/eeg"
    }
  }

  "TypeTag codec" should {
    "decode types and data" in {
      val encodedData = Chunk.byteVector(hex"2c666666660000004455d8f2445433d5445152e1444edb35")
      val ttc = TypeTagCodec()
      val res = ttc.decode(encodedData.toBitVector)
      res.require.value shouldBe List(OSCFloat(855.3898F), OSCFloat(848.8099F), OSCFloat(837.295F), OSCFloat(827.4251F))
    }
  }

}
