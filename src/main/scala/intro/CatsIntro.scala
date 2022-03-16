package intro

object CatsIntro {

  // Eq
  // simple type class to compare object

  //val aComparison: Boolean = 2 == "a string"

  // part 1 - import typeclass

  import cats.Eq

  // part 2 - import TC instances for types you need

  import cats.instances.int._

  // part 3 - use the TC api
  val intEquality: Eq[Int] = Eq[Int]
  val aTypeSafeComparison: Boolean = intEquality.eqv(2, 3)
  // val anUnsafeComparison = intEquality.eqv(2, "a stirng") -> Not compile

  // part 4 - use extension methods

  import cats.syntax.eq._

  val anotherTypeSafeComp: Boolean = 2 === 3

  // part 5 - extending TC operation to composite types

  import cats.instances.list._ // we bring Eq[List[Int]] in scope

  val aListComparison: Boolean = List(2) === List(3)

  // Part 6 - create TC instance for custom type
  case class ToyCar(model: String, price: Double)

  implicit val toyCarEq: Eq[ToyCar] = Eq.instance[ToyCar] { (car1, car2) => car1.price == car2.price }

  val compareTwoToyCars: Boolean = ToyCar("Ferrari", 1.99) === ToyCar("Lambo", 1.99)
}
