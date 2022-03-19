package part3_data_manipulation

import cats.Id
import cats.data.{Kleisli, Reader}

object Readers {

  /*
    - Configuration file => Initial data
    - a DB layer
    - a HTTP layer
    - a business logic layer
   */

  case class Configuration(dbUserName: String, dbPassword: String, host: String, port: Int, nThreads: Int, emailReplyTo: String)

  case class DBConnection(username: String, password: String) {
    def getOrderStatus(orderId: Long): String = "dispatched"

    def getLastOrderId(username: String): Long = 76543
  }

  case class HttpService(host: String, port: Int) {
    def start(): Unit = println("Server started ...")
  }

  //bootstrap phase
  val config: Configuration = Configuration("budy", "frenchie", "localhost", 8080, 9, "budy@cognincan.com")

  // cats Reader

  import cats.data.Reader

  val dbReader: Reader[Configuration, DBConnection] = Reader(conf => DBConnection(conf.dbUserName, conf.dbPassword))
  val dbConn: DBConnection = dbReader.run(config)

  val orderStatusReader: Reader[Configuration, String] = dbReader.map(_.getOrderStatus(1234))
  val orderStatus: String = orderStatusReader.run(config)

  def getLastOrderStatus(username: String): String = {
    val usersLastOrderId: Reader[Configuration, String] =
      dbReader.map(_.getLastOrderId(username))
        .flatMap(lastOderId => dbReader.map(_.getOrderStatus(lastOderId)))

    val userLastOrderIdFor: Reader[Configuration, String] = for {
      lastOrderId <- dbReader.map(_.getLastOrderId(username))
      status <- dbReader.map(_.getOrderStatus(lastOrderId))
    } yield status

    userLastOrderIdFor.run(config)
  }
  /*
    Pattern
    1. you create the initial data structure
    2. you create a reader which specifies how that data structure will be manipulated later
    3. you can then map & flatMap the reader to produce derived information
    4. when you need the final piece of information, you call run on the reader with the initial data structure
   */


  case class EmailService(emailReplyTo: String) {
    def sendEmail(address: String, contents: String) = s"From: $emailReplyTo; to: $address >>> $contents"
  }

  // TODO 1 - email a user
  def emailUser(username: String, userEmail: String): String = {
    // fetch status of their last order
    // Email them with the Email Service: "Your last order status: (status)"
    val emailReader: Reader[Configuration, EmailService] = Reader(conf => EmailService(conf.emailReplyTo))

    val serviceReader = dbReader.map(_.getLastOrderId(username))
      .flatMap(lastOrderId => dbReader.map(_.getOrderStatus(lastOrderId))
        .flatMap(orderStatus => emailReader.map(_.sendEmail(userEmail, orderStatus))))

    val serviceReaderFor: Reader[Configuration, String] = for {
      lastOrderId <- dbReader.map(_.getLastOrderId(username))
      status <- dbReader.map(_.getOrderStatus(lastOrderId))
      emailService <- emailReader
    } yield emailService.sendEmail(userEmail, s"Your last order status: $status")

    serviceReaderFor.run(config)
  }

  def main(args: Array[String]): Unit = {
    println(getLastOrderStatus("budy"))
    println(emailUser("alex", "alex@budy.com"))
  }
}
