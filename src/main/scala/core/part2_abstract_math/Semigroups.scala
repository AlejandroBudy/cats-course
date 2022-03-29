package core.part2_abstract_math

object Semigroups {

  //Semigroups combines elements of the same type

  import cats.Semigroup
  import cats.instances.int._

  val naturalIntSemigroup: Semigroup[Int] = Semigroup[Int]
  val intCombination: Int = naturalIntSemigroup.combine(2, 46) // addition

  import cats.instances.string._

  val naturalStringSemigroup: Semigroup[String] = Semigroup[String]
  val stringCombination: String = naturalStringSemigroup.combine("Hello", "world")

  def reduceInts(list: List[Int]): Int = list.reduce(naturalIntSemigroup.combine)

  // Generic API
  def reduceThings[T](list: List[T])(implicit semigroup: Semigroup[T]): T = list.reduce(semigroup.combine)

  //TODO 1: support new type
  case class Expense(id: Long, amount: Double)

  implicit val expenseSemigroup: Semigroup[Expense] = Semigroup.instance[Expense] {
    (ex1, ex2) => Expense(ex1.id + ex2.id, ex1.amount + ex2.amount)
  }

  //Extension methods from Semigroup -> |+| (combine)

  import cats.syntax.semigroup._

  val anIntSum: Int = 2 |+| 3 // requires presence of a implicit Semigroup[Int]

  //TODO 2: implement reduceThings2
  def reduceThings2[T](list: List[T])(implicit semigroup: Semigroup[T]): T = list.reduce(_ |+| _)

  def reduceThings2Shorter[T: Semigroup](list: List[T]): T = list.reduce(_ |+| _)

  def main(args: Array[String]): Unit = {
    println(intCombination)
    println(stringCombination)

    val numbers = (1 to 10).toList
    println(reduceThings(numbers))

    val numberOptions: List[Option[Int]] = numbers.map(Option(_))
    import cats.instances.option._
    println(reduceThings(numberOptions))

    val aList: List[Expense] = List(Expense(1, 2), Expense(2, 3))
    import Semigroups.expenseSemigroup
    println(reduceThings(aList)) // Expense(3, 5.0)
  }
}
