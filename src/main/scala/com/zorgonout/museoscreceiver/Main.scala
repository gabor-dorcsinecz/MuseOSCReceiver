package com.zorgonout.museoscreceiver

import java.net.InetSocketAddress

import cats.effect.{Blocker, Concurrent, ContextShift, ExitCode, IO, IOApp}
import com.zorgonout.museoscreceiver.Main.serverPort
import fs2.Stream
import fs2.io.udp.{Packet, Socket, SocketGroup}


object Main extends IOApp {

  val serverPort = 8000

  def run(args: List[String]): IO[ExitCode] = {
    serverUDPSocket()
  }

  def serverUDPSocket(): IO[ExitCode] = {
    println("Starting")
    Blocker[IO]
      .use { blocker =>
        SocketGroup[IO](blocker).use { socketGroup =>
          val cc = implicitly[Concurrent[IO]]
          val cs = implicitly[ContextShift[IO]]
          val sgr = socketGroup.open(new InetSocketAddress(serverPort))(cc, cs)
          Stream.resource(sgr).flatMap { socket =>
            handlePacketStream(socket.reads())
          }.compile.drain
        }
      }
      .as(ExitCode.Success)
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


