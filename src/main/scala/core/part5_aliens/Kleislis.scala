package core.part5_aliens

object Kleislis {

  val func1: Int => Option[String] = x => if (x % 2 == 0) Some(s"$x Is even") else None
  val func2: Int => Option[Int]    = x => Some(x * 3)

  val plainFunc1: Int => String = x => "Some value"
  val plainFunc2: Int => Int    = x => x * 3
  val plainFunc3: Int => String = plainFunc2 andThen plainFunc1

  import cats.data.Kleisli
  import cats.instances.option._

  val func1K: Kleisli[Option, Int, String] = Kleisli(func1)
  val func2K: Kleisli[Option, Int, Int]    = Kleisli(func2)
  val func3K: Kleisli[Option, Int, String] = func2K andThen func1K

  // convenience
  val multiply: Kleisli[Option, Int, Int] = func2K.map(_ * 2)

  //TODO
  import cats.Id
  type InterestingKleisli[A, B] = Kleisli[Id, A, B]

  val times2: Kleisli[Id, Int, Int] = Kleisli[Id, Int, Int](x => x * 2)
  val plus4: Kleisli[Id, Int, Int]  = Kleisli[Id, Int, Int](y => y * 4)

  val composed: Kleisli[Id, Int, Int] = times2.flatMap(x => plus4.map(y => x + y))
  val composedFor: Kleisli[Id, Int, Int] = for {
    x <- times2
    y <- plus4
  } yield x + y

  // Kleisli === Reader !!

  def main(args: Array[String]): Unit = {
    println(composedFor(4))

  }
}
