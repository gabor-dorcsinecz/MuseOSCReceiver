package com.zorgonout.museoscreceiver

import cats.effect.{ContextShift, ExitCode, IO, IOApp}


object Main extends IOApp {


  def run(args: List[String]): IO[ExitCode] = {
    //val oscServerPort = 8001
    //val oscServer = new OSCServer(8000)
    //oscServer.startServer().as(ExitCode.Success)

    val webserverPort = 8000
    val webserver = new Webserver(webserverPort)
    webserver.stream[IO].compile.drain.as(ExitCode.Success)

  }


}


