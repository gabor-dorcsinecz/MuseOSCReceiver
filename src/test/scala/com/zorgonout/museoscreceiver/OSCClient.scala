package com.zorgonout.museoscreceiver

import java.net.InetSocketAddress

import cats.effect.internals.IOAppPlatform
import cats.effect.{Blocker, Concurrent, ContextShift, ExitCode, IO, Sync}
import fs2.io.udp.{Packet, SocketGroup}
import fs2.{Stream, text}

class OSCClient {
  def run(port:Int): IO[ExitCode] = {
    implicit val ioContextShift: ContextShift[IO] = IO.contextShift(scala.concurrent.ExecutionContext.Implicits.global)
    Blocker[IO].use { blocker =>
      SocketGroup[IO](blocker).use { socketGroup =>
        client[IO](socketGroup,port).compile.drain
      }
    }.as(ExitCode.Success)
  }

  def client[F[_]: Concurrent: ContextShift](socketGroup: SocketGroup, port:Int): Stream[F,Unit] = {
    val address = new InetSocketAddress("localhost", port)
    println("Client is starting")
    Stream.resource(socketGroup.open()).flatMap { socket =>
      Stream("Hello, world!")
        .through(text.utf8Encode)
        .chunks
        .map(data => Packet(address, data))
        .through(socket.writes())
        .drain ++
        socket.reads()
          .flatMap(packet => Stream.chunk(packet.bytes))
          .through(text.utf8Decode)
          .evalMap { response =>
            Sync[F].delay(println(s"Response: $response"))
          }
    }
  }

}



//class OSCServer {
//
//  def testServer[F[_]](socketGroup: SocketGroup, port: Int): Stream[F,Unit] = {
//    Stream.resource(socketGroup.open(new InetSocketAddress(port))).flatMap { socket =>
//      handlePacketStream(socket.reads())
//    }
//  }
//
//  def handlePacketStream[F[_]](packets: Stream[F, Packet]): Stream[F, Packet] = {
//    packets.map { a => println(">> " + a.bytes.map(b => b.toChar)); a }
//  }
//}
