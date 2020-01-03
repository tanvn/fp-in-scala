package leetcode

import scala.collection.Searching._

//https://leetcode.com/problems/two-sum/
object TwoSum {

  def twoSum(nums: Array[Int], target: Int): Array[Int] = {
    nums.zipWithIndex.toSeq.view
      .map {
        case (ele, firstIndex) =>
          val secondIndex = nums.indexOf(target - ele, firstIndex + 1)
          Array[Int](firstIndex, secondIndex)
      }
      .filter(a => a(1) > -1)
      .head
  }

  def twoSumSortedArr(nums: Array[Int], target: Int): Array[Int] = {

    nums.zipWithIndex.toSeq.view
      .map {
        case (ele, firstIndex) =>
          println(ele, firstIndex)
          val remain = target - ele
          val secondIndex =
            nums.search(elem = remain, from = firstIndex, to = nums.length - 1) match {
              case Found(foundIndex) => foundIndex
              case InsertionPoint(_) => -1
            }

          Array[Int](firstIndex + 1, secondIndex + 1)
      }
      .filter(a => a(1) > 0)
      .head
  }

  def main(args: Array[String]): Unit = {
    val data1 = Array(2, 3, 4)
    val res = twoSumSortedArr(data1, 6)
    println(res(0), res(1))
  }
}
