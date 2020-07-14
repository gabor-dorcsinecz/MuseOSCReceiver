package com.zorgonout.museoscreceiver

import java.time.{LocalDateTime, ZoneOffset}

import scodec.Codec
import scodec.bits.{BitVector, ByteVector}
import scodec.codecs._

object OSCPackegeType extends Enumeration {
  val MESSAGE = Value('/'.toInt)
  val BUNDLE = Value('#'.toInt)

}

object OSC {

  sealed trait OSCPackage

  //case class OSCMessage(addressPattern: String, typeTag: String, arguments: String) extends OSCPackage
  //case class OSCBundle(timeTag: String, elements: String*) extends OSCPackage

  case class OSCMessage(data:ByteVector) extends OSCPackage
  case class OSCBundle(bundleName:String, timeTag:LocalDateTime, data:ByteVector) extends OSCPackage

  val packageTypeCodec = enumerated(uint8, OSCPackegeType)
  //val timeTagCodec = long(64)
  val timeTagSeconds = uint32
  val timeTagFraction = int32
  val timeTagCodec = (timeTagSeconds ~ timeTagFraction).xmapc{
    case (secondsSince1900,fraction) =>
      //println(s"secondsSince1900: $secondsSince1900 , fraction:$fraction")
      val secondsSince1970 = secondsSince1900 - 2208988800L
      LocalDateTime.ofEpochSecond(secondsSince1970, fraction, ZoneOffset.UTC)
  }{
    x => ((x.getSecond + 2208988800L).toInt,x.getNano)
  }

  val elementCodec = variableSizeBytes(int32, bytes)
  val bundleSize = int32

  val bundleCodec:Codec[OSCBundle] = (
    fixedSizeBytes(7,utf8) ::
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
    .|(OSCPackegeType.BUNDLE){case m:OSCBundle => m}(identity)(bundleCodec)

}