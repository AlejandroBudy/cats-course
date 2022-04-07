package effects.part2_effects

import cats.Parallel
import cats.effect.IO.Par
import cats.effect.{IO, IOApp}

object IOParallelism extends IOApp.Simple {

  //IOs are usually sequential
  val aniIO: IO[String]    = IO(s"[${Thread.currentThread().getName}] Ani")
  val kamranIO: IO[String] = IO(s"[${Thread.currentThread().getName}] Kamran")

  val composedIO: IO[String] = for {
    ani    <- aniIO
    kamran <- kamranIO
  } yield s"They love jvm $ani $kamran"

  import effects.utils._
  import cats.syntax.apply._

  val meaningOfLife: IO[Int] = IO.delay(42)
  val favLang: IO[String]    = IO.delay("Scala")

  val goalInLife: IO[String] = (meaningOfLife.debug, favLang.debug).mapN((x, y) => s"My goal at $x learn $y")

  // parallelism on IOs
  // convert a sequential IO to parallel IO
  val parIO1: IO.Par[Int]    = Parallel[IO].parallel(meaningOfLife.debug)
  val parIO2: IO.Par[String] = Parallel[IO].parallel(favLang.debug)

  import cats.effect.implicits._
  val goalParallel: Par[String] = (parIO1, parIO2).mapN((x, y) => s"My goal at $x learn $y")

  // turn back to sequential
  val goalInLine_V2: IO[String] = Parallel[IO].sequential(goalParallel)

  // shorthand
  import cats.syntax.parallel._
  val goodInLife_Short: IO[String] = (meaningOfLife.debug, favLang.debug).parMapN((x, y) => s"My goal at $x learn $y")

  // regarding failure
  val aFailure: IO[String]       = IO.raiseError(new RuntimeException("First Error!!"))
  val anotherFailure: IO[String] = IO.raiseError(new RuntimeException("Second Error!!"))
// compose success + failure
  val parallelWithFailure: IO[String] = (meaningOfLife.debug, aFailure.debug).parMapN(_ + _)

  val twoFailures: IO[String]        = (aFailure.debug, anotherFailure.debug).parMapN(_ + _)
  val twoFailuresDelayed: IO[String] = (IO(Thread.sleep(1000)) >> aFailure.debug, anotherFailure.debug).parMapN(_ + _)

  override def run: IO[Unit] = // goalInLine_V2.map(println)
    twoFailuresDelayed.debug.void
}
