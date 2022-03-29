package core.part4_applicatives

import cats.{Functor, Semigroupal}

object WeakerApplicative {

  trait MyApply[W[_]] extends Functor[W] with Semigroupal[W] {
    def product[A, B](fa: W[A], fb: W[B]): W[(A, B)] = {
      val functionWrapper: W[B => (A, B)] = map(fa)(a => (b: B) => (a, b))
      ap(functionWrapper)(fb)
    }

    def ap[B, T](wf: W[B => T])(wa: W[B]): W[T]
  }

  trait MyApplicative[W[_]] extends MyApply[W] {
    def pure[A](x: A): W[A]
  }

  import cats.Apply
  import cats.instances.option._ // implicit Apply[Option]

  val applyOption: Apply[Option] = Apply[Option]

  val funcApp: Option[Int] = applyOption.ap(Some((x: Int) => x + 1))(Some(2)) // Some(3)

  import cats.syntax.apply._ // extension methods from Apply

  val tupleOfOptions: (Option[Int], Option[Int], Option[Int]) = (Option(1), Option(2), Option(3))
  val optionOfTuple: Option[(Int, Int, Int)] = tupleOfOptions.tupled // Some((1,2,3))

  val sumOption: Option[Int] = tupleOfOptions.mapN(_ + _ + _) //Some(3)

  def main(args: Array[String]): Unit = {

  }
}
