package ordering
import scala.util.Sorting

object OrderingTest {
  def main(args: Array[String]): Unit = {
    val pairs = Array(("a", 5, 2), ("c", 3, 1), ("b", 1, 3))

    val test = Array((false, true), (false, false), (true, true),(true,false))

    // sort by 2nd element
    Sorting.quickSort(pairs)(Ordering.by[(String, Int, Int), Int](_._2))

    // sort by the 3rd element, then 1st
    Sorting.quickSort(pairs)(Ordering[(Int, String)].on(x => (x._3, x._1)))

    Sorting.quickSort(test)(Ordering[(Boolean, Boolean)].on(x => (x._1, x._2)))

    println(test)

  }

}
