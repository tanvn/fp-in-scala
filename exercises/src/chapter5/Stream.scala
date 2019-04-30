package chapter5

sealed trait Stream[+A] {
  def toList: List[A]
  def toListWithTailRec: List[A]
}

case object Empty extends Stream[Nothing] {
  override def toList: List[Nothing] = List.empty[Nothing]
  def toListWithTailRec: List[Nothing] = List.empty[Nothing]
}

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A] {
  override def toList: List[A] = List(h()) ++ t().toList

  override def toListWithTailRec: List[A] = {

    @annotation.tailrec
    def go(s: Stream[A], acc: List[A]): List[A] = s match {
      case Cons(h, t) => go(t(), acc :+ h())
      case Empty      => acc
    }
    go(this, List.empty)
  }
}

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }
  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty[A] else cons(as.head, apply(as.tail: _*))

  def main(args: Array[String]): Unit = {
    val myStream = Stream(1, 2, 3, 4, 5, 6, 7)
    println(myStream.toList)
    println(myStream.toListWithTailRec)
  }
}
