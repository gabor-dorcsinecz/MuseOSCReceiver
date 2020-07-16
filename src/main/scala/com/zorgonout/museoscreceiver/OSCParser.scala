package com.zorgonout.museoscreceiver

import java.time.{LocalDateTime, ZoneOffset}

import com.zorgonout.museoscreceiver.OSC.{OSCFloat, OSCInt, OSCType}
import scodec.{Attempt, Codec, DecodeResult, SizeBound}
import scodec.bits.{BitVector, ByteVector}
import scodec.codecs._
import shapeless.{HList, HNil}

import scala.annotation.tailrec

/**
 * The Open Sound Control 1.0 Specification
 * Open Sound Control (OSC) is an open, transport-independent, message-based protocol developed
 * for communication among computers, sound synthesizers, and other multimedia devices.
 *
 * http://opensoundcontrol.org/spec-1_0
 */

object OSC {

  object OSCPackegeType extends Enumeration {
    val MESSAGE = Value('/'.toInt)
    val BUNDLE = Value('#'.toInt)
  }

  sealed trait OSCPackage

  //case class OSCMessage(addressPattern: String, typeTag: String, arguments: String) extends OSCPackage
  //case class OSCBundle(timeTag: String, elements: String*) extends OSCPackage

  case class OSCMessage(address: String, data: ByteVector) extends OSCPackage

  case class OSCBundle(bundleName: String, timeTag: LocalDateTime, data: OSCMessage) extends OSCPackage

  val packageTypeCodec = enumerated(uint8, OSCPackegeType)
  val timeTagSeconds = uint32
  val timeTagFraction = int32
  val timeTagCodec = (timeTagSeconds ~ timeTagFraction).xmapc {
    case (secondsSince1900, fraction) =>
      val secondsSince1970 = secondsSince1900 - 2208988800L
      LocalDateTime.ofEpochSecond(secondsSince1970, fraction, ZoneOffset.UTC)
  } { x => ((x.getSecond + 2208988800L).toInt, x.getNano) }

  val elementCodec: Codec[OSCMessage] = variableSizeBytes(int32, bytes)
    .xmapc { bv => messageCodec.decode(bv.bits).require.value } { msg => messageCodec.encode(msg).require.bytes } //TODO this looks horrible
  //.narrowc(a => messageCodec.decode(a.bits).getOrElse())(m => messageCodec.encode(m).getOrElse(OSCMessage("",ByteVector.empty)))
  //.exmapc(a => messageCodec.decode(a.bits))(b => messageCodec.encode(b.value).map(_.bytes))
  //.xmapc(a => messageCodec.decode(a.bits).require)(b => messageCodec.encode(b.value).require.bytes)
  //.hlist :+ messageCodec
  val bundleSize = int32

  val bundleCodec: Codec[OSCBundle] = (
    fixedSizeBytes(7, utf8) ::
      timeTagCodec ::
      elementCodec
    ).as[OSCBundle]


  val packageCodec = discriminated[OSCPackage]
    .by(packageTypeCodec)
    //.|(OSCPackegeType.MESSAGE){case m:OSCMessage => m}(identity)(bytes)
    .|(OSCPackegeType.BUNDLE) { case m: OSCBundle => m }(identity)(bundleCodec)


  //  val messageCodec:Codec[OSCMessage] = (
  //    fixedSizeBytes(10,ascii) ::
  //  ).as[OSCMessage]
  val messageCodec: Codec[OSCMessage] = (

    FinderCodec(',', utf8) ::
      bytes
    ).as[OSCMessage]


  sealed trait OSCType
  case class OSCInt(int:Int) extends OSCType
  case class OSCFloat(float:Float) extends OSCType
  case class OSCString(string:String) extends OSCType

}

/**
 * Codec that searches for a charatcter in the ByteVector and decodes only until then
 *
 * @param value Char to search for
 * @param codec Codec to use
 * @tparam A
 */
case class FinderCodec[A](value: Char, codec: Codec[A]) extends Codec[A] {
  override def sizeBound = SizeBound.unknown

  override def encode(a: A) = Attempt.successful(BitVector.empty)

  override def decode(bv: BitVector): Attempt[DecodeResult[A]] = {
    val indexOfSeparator = bv.bytes.indexOfSlice(ByteVector(value))
    fixedSizeBytes(indexOfSeparator, codec).decode(bv)
  }
}

case class TypeTagCodec() extends Codec[Seq[OSCType]] {
  override def sizeBound: SizeBound = SizeBound.unknown

  override def encode(value: Seq[OSCType]): Attempt[BitVector] = Attempt.successful(BitVector.empty)

  override def decode(bits: BitVector): Attempt[DecodeResult[Seq[OSCType]]] = {
    val byteVector = bits.bytes.tail //Drop the comma, which marks the beginning of typetags
    val endOfOSCStringIndex = byteVector.indexOfSlice(ByteVector(0))
    val roundedTo4Bytes = math.ceil((endOfOSCStringIndex + 1).toDouble / 4).toInt * 4 //how many bytes contain all typetags (as they are padded with 0's until 32 bits)
    val (typeTags, data) = byteVector.splitAt(roundedTo4Bytes - 1)
    val values = decodeDataFromTypeTags(typeTags.toSeq, data.toBitVector)
    Attempt.successful(DecodeResult(values,BitVector.empty))
  }


  final def decodeDataFromTypeTags(typeTags: Seq[Byte], data: BitVector): Seq[OSCType] = {
    //println(s"typeTag2Codec  $typeTags    ###   $data")
    val cleanedTags = typeTags.filterNot(_ == 0)  //0's were just padding to 32 bits (OSCString)
    cleanedTags.headOption match {
      case Some('f') =>
        val x = float.decode(data)
        OSCFloat(x.require.value) +: decodeDataFromTypeTags(cleanedTags.tail, x.require.remainder)
      case Some('i') =>
        val x = int32.decode(data)
        OSCInt(x.require.value) +: decodeDataFromTypeTags(cleanedTags.tail, x.require.remainder)
      case None => Nil
      case _ => println("Unknown"); Nil
    }
  }
}

