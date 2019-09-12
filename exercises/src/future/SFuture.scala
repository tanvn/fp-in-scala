package future

import java.util.concurrent.Executors

import scala.concurrent.{ExecutionContext, ExecutionContextExecutor, Future}
import scala.util.{Failure, Success}
//import scala.concurrent.ExecutionContext.Implicits.global

object SFuture {
  implicit val exec: ExecutionContextExecutor =
    ExecutionContext.fromExecutor(Executors.newCachedThreadPool)

  def main(args: Array[String]): Unit = {
    println("hello")
    val fut = Future { Thread.sleep(2000); 21 + 21 }
    val fut1 = Future { Thread.sleep(1000); 21 + 21 }
    val fut2 = Future { Thread.sleep(1400); 23 + 23 }

    val valid = fut
      .filter(res => res > 100)
      .onComplete {
        case Failure(exception) => println(exception)
        case Success(value)     => value
      }
    println(s"valid $valid")

    val collected =
      fut.collect { case res if res > 0 => res + 46 }

    val invalid =
      fut.collect { case res if res < 0 => res + 46 }

    Thread.sleep(3000)
    println(collected.value)
    println(invalid.value)

    for {
      x <- fut1
      y <- fut2
      z <- fut
    } yield {
      val sum = x + y + z
      println(s"sum is : $sum")
    }

    val failure = Future { 42 / 0 }
    val expectedFailure = failure.failed
    expectedFailure.onComplete(res => println(s"err : $res"))
    val second = failure.transform(
      res => res * -1,
      ex => new Exception("see cause", ex)
    )
    val nonNegative = failure.transform { // Scala 2.12
      case Success(res) => Success(res.abs + 1)
      case Failure(_)   => Success(0)
    }
    Thread.sleep(1000)

    println(s"transform: $second")
    println(nonNegative.value)
    println(second.zip(nonNegative))
  }
}
