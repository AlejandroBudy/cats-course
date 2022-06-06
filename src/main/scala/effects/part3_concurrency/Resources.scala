package effects.part3_concurrency

import cats.effect.{IO, IOApp, Resource}

import java.io.{File, FileReader}
import java.util.Scanner
import scala.concurrent.duration._

object Resources extends IOApp.Simple {

  import effects.utils._
  // use-case: manage a connection lifecycle
  class Connection(url: String) {
    def open: IO[String]  = IO(s"Opening connection to $url").debug
    def close: IO[String] = IO(s"Closing connection to $url").debug
  }

  val asyncFetchUrl = for {
    fib <- (new Connection("google.es").open *> IO.sleep(Int.MaxValue.seconds)).start
    _   <- IO.sleep(1.second) *> fib.cancel
  } yield ()

  val correctAsyncFetchUrl = for {
    conn <- IO(new Connection("google.es"))
    fib  <- (conn.open *> IO.sleep(Int.MaxValue.seconds)).onCancel(conn.close.void).start
    _    <- IO.sleep(1.second) *> fib.cancel
  } yield ()

  // bracket pattern
  val bracketFetchUrl: IO[Unit] = IO(new Connection("google.es"))
    .bracket(conn => conn.open *> IO.sleep(Int.MaxValue.seconds))(conn => conn.close.void)

  val bracketProgram = for {
    fib <- bracketFetchUrl.start
    _   <- IO.sleep(1.second) *> fib.cancel
  } yield ()

  /**
    * Exercise
    * - Open a scanner
    * - Read the file line by line every 100 millis
    * - close the scanner
    * - if cancel/error close the scanner
    */
  def openFileScanner(path: String): IO[Scanner] = IO(new Scanner(new FileReader(new File(path))))

  def bracketReadFile(path: String): IO[Unit] =
    openFileScanner(path).bracket(scanner => reader(scanner))(scanner => IO(scanner.close()).void)

  def reader(scanner: Scanner): IO[Unit] =
    if (scanner.hasNextLine) IO(scanner.nextLine()) >> IO.sleep(100.millis) >> reader(scanner)
    else IO.unit

  /**
    * Resources
    */
  def connFromConfig(path: String): IO[Unit] =
    openFileScanner(path).bracket { scanner =>
      IO(new Connection(scanner.nextLine())).bracket { conn =>
        conn.open.debug >> IO.never
      }(conn => conn.close.void)
    }(scanner => IO("Closing file").debug >> IO(scanner.close()))

  val connectionResource: Resource[IO, Connection] =
    Resource.make(IO(new Connection("google.es")))(conn => conn.close.void)

  val resourceFetchUrl = for {
    fib <- (connectionResource.use(conn => conn.open) >> IO.never).start
    _   <- IO.sleep(1.second) >> fib.cancel
  } yield ()

  val simpleResource: IO[String]            = IO("Some resource")
  val usingResource: String => IO[String]   = s => IO(s"Using the string $s").debug
  val closingResource: String => IO[Unit]   = s => IO(s"finalizing resource $s").debug.void
  val usingResourceWithBracket: IO[String]  = simpleResource.bracket(usingResource)(closingResource)
  val usingResourceWithResource: IO[String] = Resource.make(simpleResource)(closingResource).use(usingResource)

  /**
    * - Exercise 1 using resource
    */
  def resourceReadFile(path: String): IO[Unit] =
    Resource.make(openFileScanner(path))(scanner => IO(scanner.close()).void).use(reader)

  def connFromConfResource(path: String): Resource[IO, Connection] =
    Resource
      .make(openFileScanner(path))(scanner => IO("Closing file").debug >> IO(scanner.close()))
      .flatMap(scanner => Resource.make(IO(new Connection(scanner.nextLine())))(conn => conn.close.void))

  val openConnection: IO[String] =
    connFromConfResource("src/main/resources/connection.txt").use(conn => conn.open)

  override def run: IO[Unit] = openConnection.void
}
