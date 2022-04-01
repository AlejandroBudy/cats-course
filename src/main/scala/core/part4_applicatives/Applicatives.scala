package core.part4_applicatives

object Applicatives {

  // Applicatives = Functors + the pure method

  import cats.Applicative
  import cats.instances.list._

  val listApplicative: Applicative[List] = Applicative[List]
  val aList: List[Int]                   = listApplicative.pure(2)

  import cats.instances.option._ // implicit Applicative[Option]

  val optionApplicative: Applicative[Option] = Applicative[Option]
  val anOption: Option[Int]                  = optionApplicative.pure(2)

  // pure extension method

  import cats.syntax.applicative._

  val aSweetList: List[Int]     = 2.pure[List] // List(2)
  val aSweetOption: Option[Int] = 2.pure[Option]

  // Monads extends Applicatives
  // Applicatives extends Functors

  def productWithApplicatives[W[_], A, B](wa: W[A], wb: W[B])(implicit
                                                              applicative: Applicative[W]): W[(A, B)] =
    applicative.tuple2(wa, wb)

  def main(args: Array[String]): Unit = {}

}
