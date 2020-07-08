package com.zorgonout.museoscreceiver

import java.net.InetSocketAddress

import cats.effect.{Blocker, Concurrent, ContextShift, ExitCode, IO, IOApp}
import fs2.Stream
import fs2.io.udp.SocketGroup


object Main extends IOApp{

  val serverPort = 8000

  def run(args: List[String]): IO[ExitCode] = {
    Blocker[IO]
      .use { blocker =>
        SocketGroup[IO](blocker).use { socketGroup =>
          println("BBBBBBBBBBBB")
          val cc = implicitly[Concurrent[IO]]
          val cs = implicitly[ContextShift[IO]]
          println("Concurrent: " + cc)
          println("ContextShift: " + cs)
          val sgr = socketGroup.open(new InetSocketAddress(serverPort))(cc,cs)
          Stream.resource(sgr).flatMap{ socket =>
            socket.reads().map{a => println(">> " + a.bytes.map(b => b.toChar));a}
          }.compile.drain
//          Stream.resource(socketGroup.open(new InetSocketAddress(serverPort))).flatMap { socket =>
//            socket.reads()
//          }.compile.drain
        //IO.unit
        }
        }
      .as(ExitCode.Success)
  }

}
