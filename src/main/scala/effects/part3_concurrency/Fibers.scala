package effects.part3_concurrency

import cats.effect.kernel.Outcome
import cats.effect.kernel.Outcome.{Canceled, Errored, Succeeded}
import cats.effect.{Fiber, FiberIO, IO, IOApp}

import scala.concurrent.duration._

object Fibers extends IOApp.Simple {

  val meaningOfLife: IO[Int] = IO.pure(42)
  val favLang: IO[String]    = IO.pure("Scala")

  import effects.utils._

  // Same threads IOs
  def simpleIOComposition(): IO[Unit] =
    for {
      _ <- meaningOfLife.debug
      _ <- favLang.debug
    } yield ()

  // Fiber similar to Thread in Cats Effect.
  // Run in a scheduler manages by Cats
  def createFiber: Fiber[IO, Throwable, String] = ???

  // The allocation of Fiber may produces side Effects hence it has to be wrapped
  val aFiber: IO[FiberIO[Int]] = meaningOfLife.debug.start

  def differentThreadIOs(): IO[Unit] =
    for {
      _ <- aFiber
      _ <- favLang.debug
    } yield ()

  // joining a fiber = wait to finish
  def runSomeOtherThread[A](io: IO[A]): IO[Outcome[IO, Throwable, A]] =
    for {
      fib    <- io.start
      result <- fib.join
    } yield result

  /*
      Possible outcomes:
      - Success with an IO
      - Failure with an exception
      - Cancelled
   */
  val someIOOnAnotherThread: IO[Outcome[IO, Throwable, Int]] = runSomeOtherThread(meaningOfLife)
  val someResultFromAnotherThread: IO[Int] = someIOOnAnotherThread.flatMap {
    case Succeeded(fa)      => fa
    case Errored(exception) => IO(0)
    case Canceled()         => IO(0)
  }

  def throwOnAnotherThread(): IO[Outcome[IO, Throwable, Int]] =
    for {
      fib    <- IO.raiseError[Int](new RuntimeException("Error!!")).start
      result <- fib.join
    } yield result

  def testCancel() = {
    val task = IO("starting").debug >> IO.sleep(1.second) >> IO("done").debug

    val taskWithCancellationHandler = task.onCancel(IO("Im being cancelled!").debug.void)
    for {
      fib    <- taskWithCancellationHandler.start
      _      <- IO.sleep(500.millis) >> IO("cancelling").debug
      _      <- fib.cancel
      result <- fib.join
    } yield result
  }
  override def run: IO[Unit] = testCancel().debug.void
}
