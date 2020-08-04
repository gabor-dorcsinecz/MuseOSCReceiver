package com.zorgonout.museoscreceiver.catseffect

import cats.effect.IO
import org.scalatest.matchers._
import org.scalatest.wordspec.AnyWordSpec
import scala.concurrent.duration._

class Basics01 extends AnyWordSpec with should.Matchers {

  "this" should {
    "work" in {
        val program = for {
        _ <-    IO{println("What's your name")}
        name <- IO.pure("John") //IO(scala.io.StdIn.readLine())  //This didn't wrork
        message <- IO.pure(s"Welcome $name")
        response <-  IO(println(message))
      } yield message

      val running = program.unsafeRunTimed(1 seconds)
      running shouldBe Some("Welcome John")
    }
  }
}
