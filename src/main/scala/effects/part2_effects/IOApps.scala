package effects.part2_effects

import cats.effect.{ExitCode, IO, IOApp}

import scala.io.StdIn

object IOApps {
  val program: IO[Unit] = for {
    line <- IO(StdIn.readLine())
    _    <- IO(println(s"you just write $line"))
  } yield ()
}

object TestApp {

  import IOApps._
  import cats.effect.unsafe.implicits.global

  def main(args: Array[String]): Unit = {
    program.unsafeRunSync()
  }
}

object FirstCEApp extends IOApp {
  import IOApps._
  override def run(args: List[String]): IO[ExitCode] = program.as(ExitCode.Success)
}

object MySimpleApp extends IOApp.Simple {
  import IOApps._
  override def run: IO[Unit] = program
}