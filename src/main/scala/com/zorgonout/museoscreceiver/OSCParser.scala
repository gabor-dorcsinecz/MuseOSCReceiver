package com.zorgonout.museoscreceiver

import java.time.{LocalDateTime, ZoneOffset}

import scodec.{Attempt, Codec, DecodeResult, SizeBound}
import scodec.bits.{BitVector, ByteVector}
import scodec.codecs._

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
    //fixedSizeBytes(12, utf8) ::
    FinderCodec(',', utf8) ::
      bytes
    ).as[OSCMessage]


}

/**
 * Codec that searches for a charatcter in the ByteVector
 * @param value Char to search for
 * @param codec Codec to use
 * @tparam A
 */
case class FinderCodec[A](value: Char,codec: Codec[A]) extends Codec[A] {
  override def sizeBound = SizeBound.unknown
  override def encode(a: A) = Attempt.successful(BitVector.empty)
  override def decode(bv: BitVector) :Attempt[DecodeResult[A]]= {
    val indexOfSeparator = bv.bytes.indexOfSlice(ByteVector(value))
    fixedSizeBytes(indexOfSeparator, codec).decode(bv)
  }
}
