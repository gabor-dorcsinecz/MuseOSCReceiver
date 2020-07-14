package com.zorgonout.museoscreceiver

import fs2.Chunk.ByteVectorChunk
import scodec.bits.ByteVector
import scodec.bits._
import fs2.Chunk

object MuseIncomingOSC {

  val example1 = List[Chunk[Byte]](
    Chunk.byteVector(hex"2362756e646c6500e2b328000e560418000000242f6d7573652f6565670000002c666666660000004455d8f2445433d5445152e1444edb35"),
    Chunk.byteVector(hex"2362756e646c6500e2b328000f1a9fbe0000002c2f6d7573652f656c656d656e74732f746f756368696e675f666f726568656164000000002c69000000000001"),
    Chunk.byteVector(hex"2362756e646c6500e2b328000f1a9fbe000000282f6d7573652f656c656d656e74732f616c7068615f6162736f6c7574650000002c6600003ee84c39"),
    Chunk.byteVector(hex"2362756e646c6500e2b328000f1a9fbe000000282f6d7573652f656c656d656e74732f626574615f6162736f6c757465000000002c6600003e766013"),
    Chunk.byteVector(hex"2362756e646c6500e2b328000f1a9fbe000000282f6d7573652f656c656d656e74732f64656c74615f6162736f6c7574650000002c6600003f11dea2"),
    Chunk.byteVector(hex"2362756e646c6500e2b328000f1a9fbe000000282f6d7573652f656c656d656e74732f74686574615f6162736f6c7574650000002c6600003ea236f8"),
    Chunk.byteVector(hex"2362756e646c6500e2b328000f1a9fbe000000282f6d7573652f656c656d656e74732f67616d6d615f6162736f6c7574650000002c660000bd1eb292"),
    Chunk.byteVector(hex"2362756e646c6500e2b328000f1a9fbe000000342f6d7573652f656c656d656e74732f686f72736573686f65000000002c666666660000003f8000003f8000003f8000003f800000"),
    Chunk.byteVector(hex"2362756e646c6500e2b328001ae147ae000000202f6d7573652f6163630000002c66666600000000be92000f3db800133f79001a"),
    Chunk.byteVector(hex"2362756e646c6500e2b328001ae147ae000000242f6d7573652f6565670000002c666666660000004451bc294452f7ff44522570444bfa42")
  )
}
