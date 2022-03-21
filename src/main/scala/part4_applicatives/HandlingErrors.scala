package part4_applicatives

import cats.Monad
import cats.MonadError

import scala.util.Try

object HandlingErrors {

  trait MyMonadError[M[_], E] extends Monad[M] {
    def raiseError[A](e: E): M[A]
  }

  type ErrorOr[A] = Either[String, A]
  val monadErrorEither: MonadError[ErrorOr, String] = MonadError[ErrorOr, String]
  val success: ErrorOr[Int] = monadErrorEither.pure(32) // Either[String, Int] == Right(32)
  val failure: ErrorOr[Int] = monadErrorEither.raiseError[Int]("Something wrong") // Either[String, Int] == Left("Somethin wrong")
  val handledError: ErrorOr[Int] = monadErrorEither.handleError(failure) {
    case "Internal error" => 69
    case _ => 22
  }

  val handledError2: ErrorOr[Int] = monadErrorEither.handleErrorWith(failure) {
    case "Bad request" => monadErrorEither.pure(23)
    case _ => Left("fake error")
  }

  val filteredSuccess: ErrorOr[Int] = monadErrorEither.ensure(success)("Number too small")(_ > 100)

  // Try and Future
  import cats.instances.try_._

  val exception = new RuntimeException("error")
  val pureException: Try[Int] = MonadError[Try, Throwable].raiseError(exception)


  def main(args: Array[String]): Unit = {}
}
