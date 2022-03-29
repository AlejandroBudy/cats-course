package effects.part2_effects

import cats.effect.IO

import scala.io.StdIn

object IOIntroduction {

  // IO
  val ourFirstIO: IO[Int] = IO.pure(43)
  val aDelayedIO: IO[Int] = IO.delay {
    println("I'm producing an integer")
    54
  }

  val aDelayed_v2: IO[Int] = IO {
    println("I'm producing an integer")
    54
  }

  val improved: IO[Int] = ourFirstIO.map(_ * 2)
  val printedMeaning: IO[Unit] = ourFirstIO.flatMap(x => IO.delay(println(x)))

  def smallApp(): IO[Unit] = for {
    line1 <- IO(StdIn.readLine())
    line2 <- IO(StdIn.readLine())
    _ <- IO.delay(println(line1 + line2))
  } yield ()

  // mapN - combine IO effects as tuples

  import cats.syntax.apply._

  val combineIO: IO[Int] = (ourFirstIO, improved).mapN(_ + _)

  def smallProgram(): IO[Unit] =
    (IO(StdIn.readLine()), IO(StdIn.readLine())).mapN(_ + _).map(println)

  def main(args: Array[String]): Unit = {

    import cats.effect.unsafe.implicits.global
    println(aDelayedIO.unsafeRunSync())

    println(smallProgram().unsafeRunSync())
  }
}
