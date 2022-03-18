package part2

import scala.annotation.tailrec

object CustomMonads {

  import cats.Monad
  implicit object OptionMonad extends Monad[Option] {
    override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = ???

    override def pure[A](x: A): Option[A] = ???

    @tailrec
    override def tailRecM[A, B](a: A)(f: A => Option[Either[A, B]]): Option[B] = f(a) match {
      case None => None
      case Some(Left(value)) => tailRecM(value)(f)
      case Some(Right(value)) => Some(value)
    }
  }
}
