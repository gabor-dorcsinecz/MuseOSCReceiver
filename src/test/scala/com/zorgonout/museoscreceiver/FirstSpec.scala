package com.zorgonout.museoscreceiver

import cats.effect.{Blocker, Concurrent, ContextShift, ExitCode, IO}
import fs2.io.udp.SocketGroup
import org.scalatest._
import org.scalatest.wordspec._
import matchers._


class FirstSpec extends AnyWordSpec  with should.Matchers {

//  "This" should {
//    "Work" in {
//      implicit val context: ContextShift[IO] = IO.contextShift(scala.concurrent.ExecutionContext.Implicits.global)
//      val port = 9999
//      Blocker[IO].use { blocker =>
//        val client = new OSCClient()
//        val server = new OSCServer()
//        val cc = implicitly[Concurrent[IO]]
//        SocketGroup[IO](blocker).use { socketGroup =>
//          val cs = client.client(socketGroup,port)(cc,context) ++ server.testServer(socketGroup,port)
//          cs.compile.drain
//        }
//      }



      //val server = Main.serverUDPSocket()
      //val client = (new OSCClient()).run(Main.serverPort)
    //client.unsafeRunSync()
      //(server *> client).unsafeRunSync()
      //      Main.serverUDPSocket().unsafeRunSync()
//      Thread.sleep(1000)
//      println("Starting client")
//      val client = new OSCClient()
//      client.run(Main.serverPort).unsafeRunSync()
//      Thread.sleep(3000)
//    }
//  }



}
