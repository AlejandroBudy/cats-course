package core.part3_data_manipulation

import cats.Eval
import cats.data.IndexedStateT

object FunctionalState {

  type MyState[S, A] = S => (S, A)

  import cats.data.State

  val countAndSay: State[Int, String] = State(currentCount => (currentCount + 1, s"Counted $currentCount"))
  val (eleven, counted10)             = countAndSay.run(10).value

  // state = "iterative" computations

  var a = 10
  a += 1
  val firstComputation = s"Added 1 to 10, obtain $a"
  a *= 5
  val secondComputation = s"Multiplied with 5, obtained $a"

  // pure FP with states
  val firstTransformation: State[Int, String]  = State((s: Int) => (s + 1, s"Added 1 to 10, obtained ${s + 1}"))
  val secondTransformation: State[Int, String] = State((s: Int) => (s * 5, s"Multiplied with 5, obtained ${s * 5}"))
  val compositeTransformation: State[Int, (String, String)] =
    firstTransformation.flatMap(firstResult => secondTransformation.map(secondResult => (firstResult, secondResult)))

  val compositeTransformation2: State[Int, (String, String)] = for {
    firstResult  <- firstTransformation
    secondResult <- secondTransformation
  } yield (firstResult, secondResult)

  // TODO: an online store
  case class ShoppingCart(items: List[String], total: Double)

  def addToCart(item: String, price: Double): State[ShoppingCart, Double] =
    State(
      (current: ShoppingCart) => (ShoppingCart(current.items :+ item, current.total + price), current.total + price))

  val boxerCart: State[ShoppingCart, Double] = for {
    _     <- addToCart("Boxing gloves", 80)
    _     <- addToCart("Bucal", 10)
    total <- addToCart("Boxing boots", 50)
  } yield total

  // TODO 2: pure mental gymnastics
  // returns a State data structure that, when run, will not change the state but will issue the value f(a)
  def inspect[A, B](f: A => B): State[A, B] = State((a: A) => (a, f(a)))

  def main(args: Array[String]): Unit = {
    println(compositeTransformation.run(10).value)
    println(compositeTransformation2.run(10).value)
    println(boxerCart.run(ShoppingCart(List(), 0)).value)
  }
}
