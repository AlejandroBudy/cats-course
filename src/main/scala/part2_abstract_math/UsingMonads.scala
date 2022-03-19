package part2_abstract_math

import scala.util.{Failure, Success, Try}

object UsingMonads {

  import cats.Monad
  import cats.instances.list._

  val monadList: Monad[List] = Monad[List]

  // applicable to Option, Try, Future, ...

  // Either is also a Monad
  val aManualEither: Either[String, Int] = Right(42)
  type ErrorOr[T] = Either[Throwable, T]

  import cats.instances.either._

  val errorMonad: Monad[ErrorOr] = Monad[ErrorOr]
  val anEither: ErrorOr[Int] = errorMonad.pure(45)

  type LoadingOr[T] = Either[String, T]

  case class OrderStatus(orderId: Long, status: String)

  def getOrderStatus(orderId: Long): LoadingOr[OrderStatus] =
    Right(OrderStatus(orderId = orderId, status = "Ready"))

  def trackLocation(orderStatus: OrderStatus): LoadingOr[String] =
    if (orderStatus.orderId > 1000) Left("Not available yet, refreshing data...")
    else Right("Madrid, SP")

  val orderId = 457L
  val loadingMonad: Monad[LoadingOr] = Monad[LoadingOr]
  val orderLocation: LoadingOr[String] = loadingMonad.flatMap(getOrderStatus(orderId))(order => trackLocation(order))

  // use extension methods

  import cats.syntax.flatMap._
  import cats.syntax.functor._

  val orderStatusBetter: LoadingOr[String] = getOrderStatus(orderId).flatMap(trackLocation)
  val orderStatusFor: LoadingOr[String] = for {
    orderStatus <- getOrderStatus(orderId)
    location <- trackLocation(orderStatus)
  } yield location

  //TODO: the service layer API of a web app
  class Connection(host: String, port: String)

  object Connection {
    def apply(): Connection = new Connection("localhost", "8080")
  }

  val config = Map(
    "host" -> "localhost",
    "port" -> "4040"
  )

  trait HttpService[M[_]] {
    def getConnection(cfg: Map[String, String]): M[Connection]

    def issueRequest(connection: Connection, payload: String): M[String]
  }

  def getResponse[M[_] : Monad](service: HttpService[M], payload: String): Any = for {
    conn <- service.getConnection(config)
    response <- service.issueRequest(conn, payload)
  } yield response

  object TryHttpService extends HttpService[Try] {
    override def getConnection(cfg: Map[String, String]): Try[Connection] =
      Success(Connection())

    override def issueRequest(connection: Connection, payload: String): Try[String] =
      if (payload.length < 43) Success("Accepted")
      else Failure(new RuntimeException("Forbidden"))
  }

  def main(args: Array[String]): Unit = {

  }
}
