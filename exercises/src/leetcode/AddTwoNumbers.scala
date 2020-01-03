package leetcode

class ListNode(var _x: Int = 0) {
  var next: ListNode = null
  var x: Int = _x
}

object Solution {

  def addTwoNumbers(l1: ListNode, l2: ListNode): ListNode = {
    var temp1 = l1
    var temp2 = l2
    val res = new ListNode()
    var curr = res
    var lastSumIsLargerThanTen = false
    while (temp1 != null && temp2 != null) {
//      println(s"l1.x = ${temp1.x}, l2.x = ${temp2.x}")
      var sum = temp1.x + temp2.x
      sum = if (lastSumIsLargerThanTen) {
        sum + 1
      } else sum
      if (sum >= 10) {
        lastSumIsLargerThanTen = true
        sum = sum - 10
      } else {
        lastSumIsLargerThanTen = false
      }
      temp1 = temp1.next
      temp2 = temp2.next
      curr.x = sum
//      println(s"sum = $sum")
      if (temp1 != null && temp2 != null) {
        curr.next = new ListNode()
        curr = curr.next
      }
    }

    while (temp1 != null) {
      curr.next = new ListNode()
      curr = curr.next
      var sum = if (lastSumIsLargerThanTen) {
        temp1.x + 1
      } else {
        temp1.x
      }
      if (sum >= 10) {
        sum = sum - 10
        lastSumIsLargerThanTen = true

      } else {
        lastSumIsLargerThanTen = false
      }
      temp1 = temp1.next
      curr.x = sum
    }
    while (temp2 != null) {
      curr.next = new ListNode()
      curr = curr.next
      var sum = if (lastSumIsLargerThanTen) {
        temp2.x + 1
      } else {
        temp2.x
      }
      if (sum >= 10) {
        sum = sum - 10
        lastSumIsLargerThanTen = true

      } else {
        lastSumIsLargerThanTen = false
      }
      curr.x = sum
      temp2 = temp2.next
    }
    if (lastSumIsLargerThanTen) {
      curr.next = new ListNode()
      curr = curr.next
      curr.x = 1
    }
    res
  }

  def main(args: Array[String]): Unit = {
    val l1 = new ListNode(9)
    l1.next = new ListNode(9)
//    l1.next.next = new ListNode(6)
//    l1.next.next.next = new ListNode(3)

    val l2 = new ListNode(9)
    l2.next = new ListNode(6)
//    l2.next.next = new ListNode(4)
//    l2.next.next.next = new ListNode(4)
//    l2.next.next.next.next = new ListNode(9)

    val l3 = addTwoNumbers(l1, l2)
    var temp = l3
    while (temp != null) {
      println(temp.x)
      temp = temp.next
    }
  }
}
