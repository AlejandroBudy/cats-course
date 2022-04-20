package effects.part2_effects

import cats.effect.{IO, IOApp}

import scala.concurrent.Future
import scala.util.Random

object IOTraversal extends IOApp.Simple {

  import scala.concurrent.ExecutionContext.Implicits.global

  def heavyComputation(string: String): Future[Int] = Future {
    Thread.sleep(Random.nextInt(1000))
    string.split(" ").length
  }

  val workLoad: List[String]     = List("I like CE", "Scala is great", "looking forward to some awesome stuff")
  val futures: List[Future[Int]] = workLoad.map(heavyComputation)

  // Traverse
  import cats.Traverse
  import cats.instances.list._

  val listTraverse: Traverse[List]    = Traverse[List]
  val singleFuture: Future[List[Int]] = listTraverse.traverse(workLoad)(heavyComputation)

  import effects.utils._

  def computeAsIO(string: String): IO[Int] =
    IO {
      Thread.sleep(Random.nextInt(1000))
      string.split(" ").length
    }.debug

  val ios: List[IO[Int]]      = workLoad.map(computeAsIO)
  val singleIO: IO[List[Int]] = listTraverse.traverse(workLoad)(computeAsIO)

  // parallel traversal
  import cats.syntax.parallel._
  // Run parallel in different threads
  val parallelSingleIO: IO[List[Int]] = workLoad.parTraverse(computeAsIO)

  def sequence[A](listOfIOs: List[IO[A]]): IO[List[A]] =
    listTraverse.traverse(listOfIOs)((a: IO[A]) => a)
  def sequence_v2[F[_]: Traverse, A](listOfIOs: F[IO[A]]): IO[F[A]] =
    Traverse[F].traverse(listOfIOs)(a => a)

  // parallel  version
  def parSequence[A](listOfIOs: List[IO[A]]): IO[List[A]] =
    listOfIOs.parTraverse(x => x)
  def parSequence_v2[F[_]: Traverse, A](listOfIOs: F[IO[A]]): IO[F[A]] =
    listOfIOs.parTraverse(x => x)

  // sequence in Traverse type class
  val singleIO_V2: IO[List[Int]]         = listTraverse.sequence(ios)
  val parallelSingleIO_V2: IO[List[Int]] = ios.parSequence // extension method from Parallel syntax

  override def run: IO[Unit] = parallelSingleIO.void
}
