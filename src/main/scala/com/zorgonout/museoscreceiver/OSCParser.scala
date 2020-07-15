package com.zorgonout.museoscreceiver

import java.time.{LocalDateTime, ZoneOffset}

import scodec.Codec
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

  case class OSCMessage(data: ByteVector) extends OSCPackage

  case class OSCBundle(bundleName: String, timeTag: LocalDateTime, data: ByteVector) extends OSCPackage

  val packageTypeCodec = enumerated(uint8, OSCPackegeType)
  val timeTagSeconds = uint32
  val timeTagFraction = int32
  val timeTagCodec = (timeTagSeconds ~ timeTagFraction).xmapc {
    case (secondsSince1900, fraction) =>
      val secondsSince1970 = secondsSince1900 - 2208988800L
      LocalDateTime.ofEpochSecond(secondsSince1970, fraction, ZoneOffset.UTC)
  } { x => ((x.getSecond + 2208988800L).toInt, x.getNano) }

  val elementCodec = variableSizeBytes(int32, bytes)
  val bundleSize = int32

  val bundleCodec: Codec[OSCBundle] = (
    fixedSizeBytes(7, utf8) ::
      timeTagCodec ::
      elementCodec
    ).as[OSCBundle]

  //  val messageCodec:Codec[OSCMessage] = (
  //    fixedSizeBytes(10,ascii) ::
  //
  //  ).as[OSCMessage]

  val packageCodec = discriminated[OSCPackage]
    .by(packageTypeCodec)
    .|(OSCPackegeType.MESSAGE){case m:OSCMessage => m.data}(a => OSCMessage(a))(bytes)
    .|(OSCPackegeType.BUNDLE) { case m: OSCBundle => m }(identity)(bundleCodec)

}