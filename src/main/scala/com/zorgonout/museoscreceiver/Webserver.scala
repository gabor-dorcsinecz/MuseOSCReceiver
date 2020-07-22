package com.zorgonout.museoscreceiver

import cats.effect.{ConcurrentEffect, ContextShift, Sync, Timer}
import cats.implicits._
import fs2.Stream
import org.http4s.HttpRoutes
import org.http4s.client.blaze.BlazeClientBuilder
import org.http4s.dsl.Http4sDsl
import org.http4s.implicits._
import org.http4s.server.blaze.BlazeServerBuilder
import org.http4s.server.middleware.Logger

import scala.concurrent.ExecutionContext


class Webserver(val port:Int) {

  def stream[F[_]: ConcurrentEffect](implicit T: Timer[F], C: ContextShift[F]): Stream[F, Nothing] = {
    val httpApp = (WebserverRoutes.helloWorldRoutes[F]()).orNotFound
    val finalHttpApp = Logger.httpApp(true, true)(httpApp)

    BlazeServerBuilder
      .apply[F](ExecutionContext.global)
      .bindHttp(port, "0.0.0.0")
      .withHttpApp(finalHttpApp)
      .serve
      .drain
  }
}


object WebserverRoutes {

  def helloWorldRoutes[F[_]: Sync](): HttpRoutes[F] = {
    val dsl = new Http4sDsl[F]{}
    import dsl._
    HttpRoutes.of[F] {
      case GET -> Root  => Ok("It works")
    }
  }
}
