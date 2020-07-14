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
  case class OSCBundle(data:ByteVector) extends OSCPackage

  val packageTypeCodec = enumerated(uint8, OSCPackegeType)
  val packageCodec = discriminated[OSCPackage]
    .by(packageTypeCodec)
    //.caseO(OSCPackegeType.MESSAGE)(pkg => pkg.)
    .|(OSCPackegeType.MESSAGE){case m:OSCMessage => m.data}(a => OSCMessage(a))(bytes)
    .|(OSCPackegeType.BUNDLE){case m:OSCBundle => m.data}(a => OSCBundle(a))(bytes)

}