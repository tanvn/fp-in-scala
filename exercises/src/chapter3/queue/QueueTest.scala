package chapter3.queue

object QueueTest {

  def main(args: Array[String]): Unit = {
    val initQ = Queue.empty[Int]
    println(initQ)
    println(initQ.isEmpty)
    val add10 = initQ.enQueue(10)
    val add20 = add10.enQueue(20)
    val add30 = add20.enQueue(30)
    println(add10)

    add20.head match {
      case None        => println("None")
      case Some(value) => println(s"head is $value")
    }
    println(add20)
    var curQ = add30
    while (!curQ.isEmpty) {
      println(curQ.head)
      curQ = curQ.deQueue()
    }
  }
}
