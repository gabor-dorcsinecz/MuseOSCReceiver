package com.zorgonout.museoscreceiver.catseffect

import java.io._

import cats.effect.concurrent.Semaphore
import cats.effect.{ContextShift, IO, _}
import cats.implicits._
import org.scalatest.matchers._
import org.scalatest.wordspec.AnyWordSpec

import scala.concurrent.ExecutionContext

class FileCopySpec extends AnyWordSpec with should.Matchers {

  case class CopyFile() {

    implicit val contextShift: ContextShift[IO] = IO.contextShift(ExecutionContext.global)

    def transmit(origin: InputStream, destination: OutputStream, buffer: Array[Byte], acc: Long): IO[Long] =
      for {
        amount <- IO(origin.read(buffer, 0, buffer.length))
        count <- if (amount > -1) IO(destination.write(buffer, 0, amount)) >> transmit(origin, destination, buffer, acc + amount)
        else IO.pure(acc) // End of read stream reached (by java.io.InputStream contract), nothing to write
      } yield count // Returns the actual amount of bytes transmitted

    def transfer(origin: InputStream, destination: OutputStream): IO[Long] =
      for {
        buffer <- IO {
          new Array[Byte](1024 * 10)
        } // Allocated only when the IO is evaluated
        total <- transmit(origin, destination, buffer, 0L)
      } yield total

    def inputStream(f: File, guard: Semaphore[IO]): Resource[IO, FileInputStream] =
      Resource.make {
        IO(new FileInputStream(f))
      } { inStream =>
        guard.withPermit {
          IO(inStream.close()).handleErrorWith(_ => IO.unit)
        }
      }

    def outputStream(f: File, guard: Semaphore[IO]): Resource[IO, FileOutputStream] =
      Resource.make {
        IO(new FileOutputStream(f))
      } { outStream =>
        guard.withPermit {
          IO(outStream.close()).handleErrorWith(_ => IO.unit)
        }
      }

    def inputOutputStreams(in: File, out: File, guard: Semaphore[IO]): Resource[IO, (InputStream, OutputStream)] =
      for {
        inStream <- inputStream(in, guard)
        outStream <- outputStream(out, guard)
      } yield (inStream, outStream)

    def copy(origin: File, destination: File): IO[Long] =
      for {
        guard <- Semaphore[IO](1)
        count <- inputOutputStreams(origin, destination, guard).use { case (in, out) => guard.withPermit(transfer(in, out)) }
      } yield count

    // The 'main' function of IOApp //
    def run(args: List[String]): IO[ExitCode] =
      for {
        _ <- if (args.length < 2) IO.raiseError(new IllegalArgumentException("Need origin and destination files"))
        else IO.unit
        orig = new File(args.head)
        dest = new File(args.tail.head)
        count <- copy(orig, dest)
        _ <- IO(println(s"$count bytes copied from ${orig.getPath} to ${dest.getPath}"))
      } yield ExitCode.Success

  }


  "it" should {
    "work" in {
      val copyFile = CopyFile()
      val res = copyFile.run("a.txt" :: "b.txt" :: Nil)
      //res.unsafeRunSync()
    }
  }
}
