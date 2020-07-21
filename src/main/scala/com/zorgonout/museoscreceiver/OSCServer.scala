package com.zorgonout.museoscreceiver

import java.net.InetSocketAddress

import cats.effect.{Blocker, Concurrent, ContextShift, ExitCode, IO, Sync}
import fs2.Stream
import fs2.io.udp.{Packet, SocketGroup}

class OSCServer(val serverPort:Int) {


  def startServer[F[_] : ContextShift : Concurrent](): F[Unit] = {
    println("Starting")
    Blocker[F]
      .use { blocker =>
        SocketGroup[F](blocker).use { socketGroup =>
          //val cc = implicitly[Concurrent[F]]
          val sgr = socketGroup.open(new InetSocketAddress(serverPort)) //(sync, cs)
          Stream.resource(sgr).flatMap { socket =>
            handlePacketStream(socket.reads())
          }.compile.drain
        }
      }
      //.as(ExitCode.Success)
  }

  def handlePacketStream[F[_]](packets: Stream[F, Packet]): Stream[F, Packet] = {
    //packets.map { a => println(">> " + a.bytes.map(b => String.format("%02x", Byte.box(b)))/*.map(b => b.toChar)*/); a }
    //packets.map { a => println(">> " + a.bytes);a}
    //println("handlePacketStream================")
    //packets.map { a => println(">> " + a.bytes.map(b => String.format("%02x", Byte.box(b))).toList.mkString); a }
    packets.map { a=> println(OSC.oscPackageCodec.decode(a.bytes.toBitVector));a}
    //    packets.map { a =>
    //      println(">> " + a.bytes.map(b => String.format("%02x", Byte.box(b))).toList.mkString)
    //      println(OSC.packageCodec.decode(a.bytes.toBitVector))
    //      a
    //    }
  }

}
