package part3_data_manipulation

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}

object Writers {

  import cats.data.Writer

  // 1 - Define them at the start
  val aWriter: Writer[List[String], Int] = Writer(List("Started something"), 45)
  // 2 - Manipulate them with pure FP
  val increasedWriter: Writer[List[String], Int] = aWriter.map(_ + 1) // values increases logs stays the same
  val aLogsWriter: Writer[List[String], Int] = aWriter.mapWritten(_ :+ "Found something interesting") // value stays the same, log changes
  val aWriterWithBoth: Writer[List[String], Int] = aWriter.bimap(_ :+ "Found something interesting", _ + 1) //both change
  val aWriterWithBoth2: Writer[List[String], Int] = aWriter.mapBoth { (logs, value) =>
    (logs :+ "found something interesting", value + 1)
  }
  // 3- dump either value or logs
  val desiredValue: Int = aWriter.value
  val logs: List[String] = aWriter.written
  val (l, v) = aWriter.run

  val writerA: Writer[Vector[String], Int] = Writer(Vector("Log A1", "Log A2"), 10)
  val writerB: Writer[Vector[String], Int] = Writer(Vector("Log B1"), 30)
  // combine by semigroup
  val compositeWriter: Writer[Vector[String], Int] = for {
    va <- writerA
    vb <- writerB
  } yield va + vb

  //reset the logs
  val anEmptyWriter: Writer[List[String], Int] = aWriter.reset

  // TODO 1: rewrite a function which "prints" things with writers
  def countAndSay(n: Int): Unit = {
    if (n <= 0) println("starting!")
    else {
      countAndSay(n - 1)
      println(n)
    }
  }

  def countAndLog(n: Int): Writer[Vector[String], Int] = {
    if (n <= 0) Writer(Vector("Starting"), 0)
    else countAndLog(n - 1).flatMap(_ => Writer(Vector(s"$n"), n))
  }


  // Benefit #1: we work with pure FP

  // TODO 2: rewrite this method with writers
  def naiveSum(n: Int): Int = {
    if (n <= 0) 0
    else {
      println(s"Now at $n")
      val lowerSum = naiveSum(n - 1)
      println(s"Computed sum(${n - 1}) = $lowerSum")
      lowerSum + n
    }
  }

  def sumWithLogs(n: Int): Writer[Vector[String], Int] = {
    if (n <= 0) Writer(Vector(), 0)
    else for {
      _ <- Writer(Vector(s"Now at $n"), n)
      lowerSum <- sumWithLogs(n - 1)
      _ <- Writer(Vector(s"Computed sum(${n - 1}) = $lowerSum"), n)
    } yield lowerSum + n
  }

  // Benefit #2: Writers can keep logs separate on multiple threads

  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))

  def main(args: Array[String]): Unit = {
    println(compositeWriter.run)
    countAndLog(10).written.foreach(println)

    Future(naiveSum(100)).foreach(println)
    Future(naiveSum(100)).foreach(println)

    val sumFuture1 = Future(sumWithLogs(100))
    val sumFuture2 = Future(sumWithLogs(100))
    val logs1 = sumFuture1.map(_.written) // logs from thread 1
    val logs2 = sumFuture2.map(_.written) // logs from thread 2

    println(logs1)

  }
}
