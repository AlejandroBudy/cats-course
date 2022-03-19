package part3_data_manipulation

object Evaluation {

  /*
  Cats makes the distinction between
  - evaluating an expression eagerly
  - evaluating lazily and every time you request it
  - evaluating lazily and keeping the value (memoizing)
 */

  import cats.Eval

  //Eval the expression even before using it
  val instant: Eval[Int] = Eval.now {
    println("Computing now!")
    34567
  }

  // Evaluating always
  val redoEval: Eval[Int] = Eval.always {
    println("Computing again")
    4235
  }

  // Value stored
  val delayedEval: Eval[Int] = Eval.later {
    println("Computing later!")
    12345678
  }

  val composeEvaluation: Eval[Int] = instant.flatMap(value1 => delayedEval.map(value2 => value1 + value2))

  val anotherComposeEvaluation: Eval[Int] = for {
    value1 <- instant
    value2 <- delayedEval
  } yield value1 + value2

  // TODO 1
  val evalEx1: Eval[Int] = for {
    a <- delayedEval
    b <- redoEval
    c <- instant
    d <- redoEval
  } yield a + b + c + d

  // "remember" a compute value
  val dontRecompute: Eval[Int] = redoEval.memoize

  val tutorial: Eval[String] = Eval.always {
    println("Step 1...")
    "say hello"
  }.map { step1 =>
    println("Step 2 ...")
    s"$step1 then put your left hand on the neck"
  }.memoize
    .map { steps12 =>
      println("Step 3, more complicated")
      s"$steps12 then with the right hand strike the strings"
    }

  // TODO 2:
  def defer[T](eval: => Eval[T]): Eval[T] = Eval.later(()).flatMap(_ => eval)

  // TODO 3
  def reverseList[T](list: List[T]): List[T] =
    if (list.isEmpty) list
    else reverseList(list.tail) :+ list.head

  def reverseEval[T](list: List[T]): Eval[List[T]] =
    if (list.isEmpty) Eval.later(list)
    else Eval.defer(reverseEval(list.tail).map(_ :+ list.head))

  def main(args: Array[String]): Unit = {
    //    println(instant.value)
    //    println(redoEval.value)
    //    println(redoEval.value)
    //    println(delayedEval.value)
    //    println(delayedEval.value)

    //    println(composeEvaluation.value)
    //    println(composeEvaluation.value)

    //    println(evalEx1.value)
    //    println("----")
    //    println(evalEx1.value)

    //    println(dontRecompute.value)
    //    println(dontRecompute.value)
    println(tutorial.value)
    println(tutorial.value)
  }
}
