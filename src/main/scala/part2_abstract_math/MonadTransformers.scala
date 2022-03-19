package part2_abstract_math

import cats.implicits.catsSyntaxSemigroup

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, ExecutionContextExecutorService, Future}

object MonadTransformers {

  def sumAllOptions(values: List[Option[Int]]): Int = ???

  import cats.data.OptionT
  import cats.instances.list._ // fetch an implicit OptionT[List]

  val listOfNumberOptions: OptionT[List, Int] = OptionT(List(Option(1), Option(2)))
  val listOfCharOptions: OptionT[List, Char] = OptionT(List(Option('a'), Option('b')))
  val listOfTuples: OptionT[List, (Int, Char)] = for {
    char <- listOfCharOptions
    int <- listOfNumberOptions
  } yield (int, char)

  // either transformer

  import cats.data.EitherT

  val listOfEithers: EitherT[List, String, Int] = EitherT(List(Left("error"), Right(1)))
  implicit val ec: ExecutionContextExecutorService = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))
  val futureOfEither: EitherT[Future, String, Int] = EitherT(Future[Either[String, Int]](Left("done")))

  /*
   TODO exercise
   We have a multi-machine cluster for your business which will receive a traffic surge following a media appearance.
   We measure bandwidth in units.
   We want to allocate TWO of our servers to cope with the traffic spike.
   We know the current capacity for each server and we know we'll hold the traffic if the sum of bandwidths is > 250.
  */
  val bandwidths = Map(
    "server1.rockthejvm.com" -> 50,
    "server2.rockthejvm.com" -> 300,
    "server3.rockthejvm.com" -> 170
  )
  type AsyncResponse[T] = EitherT[Future, String, T]

  def getBandWidth(server: String): AsyncResponse[Int] = bandwidths.get(server) match {
    case None => EitherT(Future[Either[String, Int]](Left("Not found")))
    case Some(b) => EitherT(Future[Either[String, Int]](Right(b)))
  }

  import cats.instances.future._

  def canWithstandSurge(s1: String, s2: String): AsyncResponse[Boolean] =
    for {
      band1 <- getBandWidth(s1)
      band2 <- getBandWidth(s2)
    } yield band1 + band2 > 250

  def generateTrafficSpikeReport(s1: String, s2: String): AsyncResponse[String] =
    canWithstandSurge(s1, s2).transform {
      case Right(_) => Right("Can handle")
      case Left(_) => Left("Error")
    }

}