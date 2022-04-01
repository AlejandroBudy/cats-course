package core.part2_abstract_math

object Monoids {

  import cats.Semigroup
  import cats.instances.int._
  import cats.syntax.semigroup._

  val numbers: List[Int] = (1 to 100).toList

  // |+| is always associative
  val sumLeft: Int  = numbers.foldLeft(0)(_ |+| _)
  val sumRight: Int = numbers.foldRight(0)(_ |+| _)

  //define a general API
  //  def combineFold[T](list: List[T])(implicit semigroup: Semigroup[T]): T =
  //    list.foldLeft(/* what is the starting value */)(_ |+| _)

  // MONOID

  import cats.Monoid

  val intMonoid: Monoid[Int] = Monoid[Int]
  val combineInt: Int        = intMonoid.combine(23, 99)
  val zero: Int              = intMonoid.empty // 0

  import cats.instances.string._

  val emptyString: String   = Monoid[String].empty
  val combineString: String = Monoid[String].combine("I like ", "Monoids!")

  import cats.instances.option._ // construct an implicit Monoid[Option[Int]]

  val emptyOption: Option[Int]        = Monoid[Option[Int]].empty
  val emptyOptionCombine: Option[Int] = Monoid[Option[Int]].combine(Option(2), Option.empty[Int])

  // extension methods for monoid -> |+|

  val combineOptionFancy: Option[Int] = Option(3) |+| Option(7)

  //TODO implement reduce by fold generic
  def combineFold[T](list: List[T])(implicit monoid: Monoid[T]): T =
    list.fold(monoid.empty)(_ |+| _)

  def combineFoldShorter[T: Monoid](list: List[T]): T = list.fold(Monoid[T].empty)(_ |+| _)

  // TODO 2: combine a list of phonebooks as Maps[String, Int]
  val phoneBooks = List(
    Map(
      "Budy" -> 123,
      "kira" -> 234
    ),
    Map(
      "Kitty" -> 567
    )
  )

  import cats.instances.map._

  combineFoldShorter(phoneBooks)

  // TODO 3 shopping cart
  case class ShoppingCart(items: List[String], total: Double)

  implicit val shoppingCartMonoid: Monoid[ShoppingCart] = Monoid.instance(
    ShoppingCart(List(), 0.0),
    (x, y) => ShoppingCart(x.items ++ y.items, x.total + y.total)
  )

  def checkout(shoppingCarts: List[ShoppingCart]): ShoppingCart = combineFoldShorter(shoppingCarts)

  def main(args: Array[String]): Unit = {
    println(sumLeft)
    println(sumRight)

    println(combineFold(numbers))
    println(combineFoldShorter(numbers))

    println(combineFoldShorter(phoneBooks))
  }

}
