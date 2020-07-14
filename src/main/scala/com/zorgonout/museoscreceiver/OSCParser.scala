package com.zorgonout.museoscreceiver

import scodec.Codec
import scodec.codecs._

sealed trait OSCPackage

case class OSCMessage(addressPattern:String,typeTag:String,arguments:String *) extends OSCPackage

case class OSCBundle(timeTag:String,elements:String *) extends OSCPackage

