package part4_applicatives

import cats.{Applicative, Monad}

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}

object Traversing {

  implicit val ex: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))
  val servers: List[String] = List("my-server", "other-server", "another-server")

  def getBandWith(hostname: String): Future[Int] = Future(hostname.length + 80)


  val allBandwidths: Future[List[Int]] = servers.foldLeft(Future(List.empty[Int])) {
    (acc, hostname) =>
      val bandFuture: Future[Int] = getBandWith(hostname)
      for {
        accBandwidths <- acc
        band <- bandFuture
      } yield accBandwidths :+ band
  }

  val allBandwidthsTraverse: Future[List[Int]] = Future.traverse(servers)(getBandWith)
  val allBandwidthsSequence: Future[List[Int]] = Future.sequence(servers.map(getBandWith))

  // TODO 1

  import cats.syntax.applicative._
  import cats.syntax.flatMap._
  import cats.syntax.functor._

  def listTraverse[F[_] : Monad, A, B](list: List[A])(func: A => F[B]): F[List[B]] =
    list.foldLeft(List.empty[B].pure[F]) {
      (accum, element) =>
        val elementWrapper: F[B] = func(element)
        for {
          acc <- accum
          elem <- elementWrapper
        } yield acc :+ elem
    }
  import cats.syntax.apply._

  // less strict using Applicative instead of Monads
  def listTraverseMin[F[_] : Applicative, A, B](list: List[A])(func: A => F[B]): F[List[B]] =
    list.foldLeft(List.empty[B].pure[F]) {
      (accum, element) =>
        val elementWrapped = func(element)
        (accum, elementWrapped).mapN(_ :+ _)
    }

}
