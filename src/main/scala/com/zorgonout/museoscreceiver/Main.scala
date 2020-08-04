package com.zorgonout.museoscreceiver

import cats.effect.{ContextShift, ExitCode, IO, IOApp}
import scala.concurrent.duration._
import cats.implicits._

object Main extends IOApp {


  def run(args: List[String]): IO[ExitCode] = {
    //val oscServerPort = 8001
    //val oscServer = new OSCServer(8000)
    //oscServer.startServer().as(ExitCode.Success)

    val webserverPort = 8000
    val webserver = new Webserver(webserverPort)
    val webIO = webserver.stream[IO].compile.drain.as(ExitCode.Success)

    val read =        IO.shift *> IO{scala.io.StdIn.readLine("Press ENTER to stop")}
    //val wait =        IO.shift *> IO.sleep(5 seconds)

    for {
      _ <-        IO.delay{println(s"Server Started on $webserverPort")}
      //_ <-        List(read,wait).parSequence  //The timer will start immediately, but the whole will not finish until Enter is pressed
      _ <-        IO.race(read,webIO)   //Either times out, or shuts down when Enter is pressed
      _ <-        IO{println(s"Shutting Down")}
      exitCode <- IO.pure(ExitCode.Success)
    } yield exitCode

  }


}


