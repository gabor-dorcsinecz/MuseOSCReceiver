package com.zorgonout.museoscreceiver.catseffect

import scala.concurrent.ExecutionContext.Implicits.global
import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits._

object CatsIOParallelApp extends IOApp {

  override def run(args: List[String]): IO[ExitCode] = {
    def printThreadId(msg: String) =
      println(s"${Thread.currentThread.getId} : $msg")

    def io(x: String) = IO {
      println(s"starting $x")
      printThreadId(x)
      Thread.sleep(100)
      println(s"finishing $x")
      "XX " + x + " XX"
    }

    printThreadId("main start")

    val i1 = IO.shift *> io("one")
    val i2 = IO.shift *> io("two")
    val i3 = IO.shift *> io("three")

    val result = List(i1, i2, i3).parSequence

    printThreadId("main wait")
    println(result.unsafeRunSync())
    printThreadId("main end")
    IO.pure(ExitCode.Success)

    /* THE OUTPUT IS
    Thread-704 : main start
Thread-704 : main wait
starting one
starting three
starting two
Thread-706 : two
Thread-707 : three
Thread-705 : one
finishing three
finishing one
finishing two
List(XX one XX, XX two XX, XX three XX)
Thread-704 : main end

     */
  }
}

