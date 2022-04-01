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

  val improved: IO[Int]        = ourFirstIO.map(_ * 2)
  val printedMeaning: IO[Unit] = ourFirstIO.flatMap(x => IO.delay(println(x)))

  def smallApp(): IO[Unit] =
    for {
      line1 <- IO(StdIn.readLine())
      line2 <- IO(StdIn.readLine())
      _     <- IO.delay(println(line1 + line2))
    } yield ()

  // mapN - combine IO effects as tuples

  import cats.syntax.apply._

  val combineIO: IO[Int] = (ourFirstIO, improved).mapN(_ + _)

  def smallProgram(): IO[Unit] =
    (IO(StdIn.readLine()), IO(StdIn.readLine())).mapN(_ + _).map(println)

  /** Exercises
    */
  // Sequence two IO and take the result of the last
  def sequenceTakeLast[A, B](ioa: IO[A], iob: IO[B]): IO[B] = ioa.flatMap(_ => iob)

  def sequenceTakeLastShort[A, B](ioa: IO[A], iob: IO[B]): IO[B] = ioa *> iob

  // Evaluation is lazy
  def sequenceTakeLastShort_V2[A, B](ioa: IO[A], iob: IO[B]): IO[B] = ioa >> iob

  // Sequence two IO and take the result of the first
  def sequenceTakeFirst[A, B](ioa: IO[A], iob: IO[B]): IO[A] =
    for {
      a <- ioa
      _ <- iob
    } yield a

  def sequenceTakeFirstShort_V2[A, B](ioa: IO[A], iob: IO[B]): IO[A] = ioa <* iob

  def forever[A](ioa: IO[A]): IO[A] = ioa.flatMap(_ => forever(ioa))

  def foreverV2[A](ioa: IO[A]): IO[A] = ioa.foreverM

  def convert[A, B](ioa: IO[A], value: B): IO[B] = ioa.map(_ => value)

  def asUnit[A](ioa: IO[A]): IO[Unit] = ioa.as(())

  def asUnitv2[A](ioa: IO[A]): IO[Unit] = ioa.void

  // This stackoverflow
  def sumIO(n: Int): IO[Int] =
    if (n <= 0) IO.pure(0)
    else sumIO(n - 1).map(_ + n)

  // Based on flatMap => lazy evaluation = not crashing
  def sumIOSafe(n: Int): IO[Int] =
    if (n <= 0) IO.pure(0)
    else
      for {
        lastNumber <- IO(n)
        prevNum    <- sumIOSafe(n - 1)
      } yield lastNumber + prevNum

  def main(args: Array[String]): Unit = {

    import cats.effect.unsafe.implicits.global
    println(aDelayedIO.unsafeRunSync())

    println(smallProgram().unsafeRunSync())
  }
}
