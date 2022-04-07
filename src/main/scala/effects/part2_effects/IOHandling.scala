package effects.part2_effects

import cats.{Applicative, Apply}
import cats.effect.IO

import scala.util.Try

object IOHandling {

  val aFailedCompute: IO[Int] = IO.delay(throw new RuntimeException("Error!"))
  val aFailure: IO[Int]       = IO.raiseError(new RuntimeException("a proper fail"))

  val dealWithIt: IO[AnyVal] = aFailure.handleErrorWith {
    case _: RuntimeException => IO.delay(println("Im still here"))
  }

  val effectsAsEither: IO[Either[Throwable, Int]] = aFailure.attempt
  //redeem: Transform the failure and the success in one go
  val resultAsString: IO[String] = aFailure.redeem(ex => "Failed", x => s"Success $x")
  //redeemWith
  val resultAsEffect: IO[Unit] = aFailure.redeemWith(thr => IO(println("Error")), x => IO(println("Success")))

  /*
      Exercises
   */
  // 1- Construct potentially failed IOs from standard data types( Option, Try, Either)
  def option2IO[A](option: Option[A])(ifEmpty: Throwable): IO[A] = ???//option.fold(IO.raiseError(ifEmpty))(IO.pure(_))
  def try2IO[A](aTry: Try[A]): IO[A]                             = aTry.fold(IO.raiseError, IO.pure)
  def either2IO[A](anEither: Either[Throwable, A]): IO[A]        = anEither.fold(IO.raiseError, IO.pure)

  // 2 - handleError, handleErrorWith
  def handleIOError[A](io: IO[A])(handler: Throwable => A): IO[A]         = io.redeem(handler, identity)
  def handleIOErrorWith[A](io: IO[A])(handler: Throwable => IO[A]): IO[A] = io.redeemWith(handler, IO.pure)

  def main(args: Array[String]): Unit = {
    import cats.effect.unsafe.implicits.global
    println(resultAsString.unsafeRunSync())
    resultAsEffect.unsafeRunSync()
  }
}
