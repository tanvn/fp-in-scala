package general

import scala.annotation.tailrec

object ListTest {

  def main(args: Array[String]): Unit = {
    List(1, 2, 3).reduceLeft { _ + _ }
    val mainList = List(3, 2, 1)
    val with4 = 4 :: mainList // re-uses mainList, costs one :: instance
    val with42 = 42 :: mainList // also re-uses mainList, cost one :: instance
    val shorter = mainList.tail // costs nothing as it uses the same 2::1::Nil instances as mainListval mainList = List(3, 2, 1)

  }

  // tail-recursive solution
  def sum(list: List[Int]): Int = {
    @tailrec
    def sumWithAccumulator(list: List[Int], currentSum: Int): Int = {
      list match {
        case Nil     => currentSum
        case x :: xs => sumWithAccumulator(xs, currentSum + x)
      }
    }
    sumWithAccumulator(list, 0)
  }
}
