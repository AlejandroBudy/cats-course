package core.part2_abstract_math

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}

object Monads {

  // lists
  val numberList = List(1, 2, 3)
  val charList = List('a', 'b', 'c')

  //TODO 1.1: how do you create all combination of (number, char)?

  val combinationList: List[(Int, Char)] = numberList.flatMap(n => charList.map((n, _)))
  val combinationListFor: List[(Int, Char)] = for {
    n <- numberList
    c <- charList
  } yield (n, c)

  // options

  val numberOptions: Option[Int] = Option(2)
  val charOption: Option[Char] = Option('b')
  val combinationOption: Option[(Int, Char)] = for {
    n <- numberOptions
    c <- charOption
  } yield (n, c)
  val combinationOptionMap: Option[(Int, Char)] =
    numberOptions.flatMap(n => charOption.map((n, _)))

  //futures

  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))
  val numberFuture: Future[Int] = Future(42)
  val charFuture: Future[Char] = Future('C')
  val combinedFuture: Future[(Int, Char)] = numberFuture.flatMap(n => charFuture.map((n, _)))
  val combinedFutureFor: Future[(Int, Char)] = for {
    n <- numberFuture
    c <- charFuture
  } yield (n, c)

  /*
  Pattern
  - wrapping the value into Monadic value
  - the flatMap mechanism

  MONADS
   */

  trait MyMonad[M[_]] {
    def pure[A](value: A): M[A]

    def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]

    //Implement
    def map[A, B](ma: M[A])(f: A => B): M[B] =
      flatMap(ma)(a => pure(f(a)))
  }

  // Cats Monad

  import cats.Monad
  import cats.instances.option._

  val optionMonad: Monad[Option] = Monad[Option]
  val anOptionMonad: Option[Int] = optionMonad.pure(4) // Option(4) == Some(4)
  val aTransformedOption: Option[Int] =
    optionMonad.flatMap(anOptionMonad)(x => if (x % 3 == 0) Some(x + 1) else None)

  import cats.instances.list._

  val listMonad: Monad[List] = Monad[List]
  val aList: List[Int] = listMonad.pure(3)
  val aTransformedList: List[Int] = listMonad.flatMap(aList)(x => List(x, x + 1))

  //TODO 2: Use Monad[Future]

  import cats.instances.future._

  val aFutureMonad: Monad[Future] = Monad[Future]
  val aFuture: Future[Int] = aFutureMonad.pure(4)
  val aTransformedFuture: Future[String] = aFutureMonad.flatMap(aFuture)(x => Future(s"$x"))

  def getPairsF[M[_], A, B](ma: M[A], mb: M[B])(implicit monad: Monad[M]): M[(A, B)] =
    monad.flatMap(ma)(a => monad.map(mb)(b => (a, b)))

  // extensions methods - weirder imports - pure, flatMap

  import cats.syntax.applicative._ // pure is here

  val oneOption: Option[Int] = 1.pure[Option] // implicit Monad[Option] will be used
  val oneList: List[Int] = 1.pure[List]

  import cats.syntax.flatMap._ // flatMap is here

  val oneOptionTransformed: Option[Int] = oneOption.flatMap(x => (x + 1).pure[Option])

  //TODO 3: Implement map method in MyMonad
  // def map[A, B](ma: M[A])(f: A => B): M[B] =
  // flatMap(ma)(a => pure(f(a)))

  // Monads extends Functors

  import cats.syntax.functor._

  val oneOptionMapped: Option[Int] = Monad[Option].map(Option(2))(_ + 1)
  val oneOptionMapped2: Option[Int] = oneOption.map(_ + 2)


  def getPairsShorter[M[_] : Monad, A, B](ma: M[A], mb: M[B]): M[(A, B)] =
    for {
      a <- ma
      b <- mb
    } yield (a, b)

  def getPairsShorterMap[M[_] : Monad, A, B](ma: M[A], mb: M[B]): M[(A, B)] =
    ma.flatMap(a => mb.map((a, _)))

  def main(args: Array[String]): Unit = {

  }
}
