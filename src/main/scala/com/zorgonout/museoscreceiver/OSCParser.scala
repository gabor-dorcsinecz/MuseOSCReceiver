package com.zorgonout.museoscreceiver

import scodec.Codec
import scodec.bits.ByteVector
import scodec.codecs._

object OSCPackegeType extends Enumeration {
  val MESSAGE = Value('/'.toInt)
  val BUNDLE = Value('#'.toInt)

}

object OSC {

  sealed trait OSCPackage

  //case class OSCMessage(addressPattern: String, typeTag: String, arguments: String*) extends OSCPackage
  //case class OSCBundle(timeTag: String, elements: String*) extends OSCPackage

  case class OSCMessage(data:ByteVector) extends OSCPackage
  case class OSCBundle(data:String, timeTag:Long) extends OSCPackage

  val packageTypeCodec = enumerated(uint8, OSCPackegeType)

  val timeTagCodec = ulong(8)

  //constant(ByteVector("bundle".getBytes("ascii")))  //Error:(28, 7) Could not prove that Unit can be converted to/from com.zorgonout.museoscreceiver.OSC.OSCBundle).as[OSCBundle]
  val bundleCodec:Codec[OSCBundle] = (
    fixedSizeBytes(7,utf8) ::
    timeTagCodec
  ).as[OSCBundle]


  val packageCodec = discriminated[OSCPackage]
    .by(packageTypeCodec)
    //.caseO(OSCPackegeType.MESSAGE)(pkg => pkg.)
    .|(OSCPackegeType.MESSAGE){case m:OSCMessage => m.data}(a => OSCMessage(a))(bytes)
    .|(OSCPackegeType.BUNDLE){case m:OSCBundle => m}(identity)(bundleCodec)

}