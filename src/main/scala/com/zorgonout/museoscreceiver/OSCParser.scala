package com.zorgonout.museoscreceiver

import java.time.{LocalDateTime, ZoneOffset}

import com.zorgonout.museoscreceiver.OSC.{OSCFloat, OSCInt, OSCType}
import scodec.bits.{BitVector, ByteVector}
import scodec.codecs._
import scodec.{Attempt, Codec, DecodeResult, SizeBound}


/**
 * The Open Sound Control 1.0 Specification
 * Open Sound Control (OSC) is an open, transport-independent, message-based protocol developed
 * for communication among computers, sound synthesizers, and other multimedia devices.
 *
 * Specification: http://opensoundcontrol.org/spec-1_0
 * An implementation in swift: https://github.com/sammysmallman/OSCKit/tree/master/Sources/OSCKit
 */

object OSC {

  sealed trait OSCType

  case class OSCInt(int: Int) extends OSCType

  case class OSCFloat(float: Float) extends OSCType

  case class OSCString(string: String) extends OSCType

  //TODO Add TimeTag type and it's parser

  object OSCPackegeType extends Enumeration {
    val MESSAGE = Value('/'.toInt)
    val BUNDLE = Value('#'.toInt)
  }

  sealed trait OSCPackage

  case class OSCMessage(address: String, data: Seq[OSCType]) extends OSCPackage

  case class OSCBundle(bundleName: String, timeTag: LocalDateTime, data: OSCMessage) extends OSCPackage

  val packageTypeCodec = enumerated(uint8, OSCPackegeType)
  val timeTagSeconds = uint32
  val timeTagFraction = uint32
  val timeTagCodec = (timeTagSeconds ~ timeTagFraction).xmapc {
    case (secondsSince1900, fraction) =>
      val secondsSince1970 = secondsSince1900 - 2208988800L
      val nanoSeconds = (fraction / 4.29497f).toInt  //TimeTag has ~200 picosecond accuracy on 32 bits you can store 4294967295 data, but only 999999999 allowed by java LocalDateTime
      LocalDateTime.ofEpochSecond(secondsSince1970, nanoSeconds, ZoneOffset.UTC)
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


  val messageCodec: Codec[OSCMessage] = (
    FinderCodec(',') ::
      TypeTagCodec()
    ).as[OSCMessage]


}

//Codec that searches for a charatcter in the ByteVector and decodes only until then
case class FinderCodec(value: Char) extends Codec[String] {
  override def sizeBound = SizeBound.unknown

  override def encode(a: String) = Attempt.successful(BitVector.empty)

  override def decode(bv: BitVector): Attempt[DecodeResult[String]] = {
    val indexOfSeparator = bv.bytes.indexOfSlice(ByteVector(value))
    fixedSizeBytes(indexOfSeparator, utf8).decode(bv).map(_.map(_.trim))
  }
}

// Codec that parses the type tags, and based on those tags, extracts the data
case class TypeTagCodec() extends Codec[Seq[OSCType]] {
  override def sizeBound: SizeBound = SizeBound.unknown

  override def encode(value: Seq[OSCType]): Attempt[BitVector] = Attempt.successful(BitVector.empty)

  override def decode(bits: BitVector): Attempt[DecodeResult[Seq[OSCType]]] = {
    val byteVector = bits.bytes.tail //Drop the comma, which marks the beginning of typetags
    val endOfOSCStringIndex = byteVector.indexOfSlice(ByteVector(0))
    val roundedTo4Bytes = math.ceil((endOfOSCStringIndex + 1).toDouble / 4).toInt * 4 //how many bytes contain all typetags (as they are padded with 0's until 32 bits)
    val (typeTags, data) = byteVector.splitAt(roundedTo4Bytes - 1)
    val values = decodeDataFromTypeTags(typeTags.toSeq, data.toBitVector)
    Attempt.successful(DecodeResult(values, BitVector.empty))
  }


  final def decodeDataFromTypeTags(typeTags: Seq[Byte], data: BitVector): Seq[OSCType] = {
    //println(s"typeTag2Codec  $typeTags    ###   $data")
    val cleanedTags = typeTags.filterNot(_ == 0) //0's were just padding to 32 bits (OSCString)
    cleanedTags.headOption match {
      case Some('f') =>
        val x = float.decode(data)
        OSCFloat(x.require.value) +: decodeDataFromTypeTags(cleanedTags.tail, x.require.remainder)
      case Some('i') =>
        val x = int32.decode(data)
        OSCInt(x.require.value) +: decodeDataFromTypeTags(cleanedTags.tail, x.require.remainder)
      //TODO add more types (like timetags and strings)
      case None =>
        Nil
      case _ =>
        println("Unknown")
        Nil
    }
  }
}

