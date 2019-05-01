package chapter5

sealed trait Stream[+A] {
  def toList: List[A]
  def toListWithTailRec: List[A]

  def take(n: Int): Stream[A]
  def drop(n: Int): Stream[A]

  def takeWhile(p: A => Boolean): Stream[A]

}

case object Empty extends Stream[Nothing] {
  override def toList: List[Nothing] = List.empty[Nothing]
  def toListWithTailRec: List[Nothing] = List.empty[Nothing]

  override def take(n: Int): Stream[Nothing] = Empty

  override def drop(n: Int): Stream[Nothing] = Empty

  override def takeWhile(p: Nothing => Boolean): Stream[Nothing] = Empty
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

  override def take(n: Int): Stream[A] =
    if (n <= 0) this
    else
      this match {
        case Cons(hd, tl) if n > 1 =>
          Cons(hd, () => tl().take(n - 1))
        case Cons(hd, _) if n == 1 =>
          Cons(hd, () => Stream.empty[A])

      }

  def takeReverse(n: Int): Stream[A] = {

    @annotation.tailrec
    def go(remain: Int, acc: Stream[A], origin: Stream[A]): Stream[A] =
      if (remain == 0) acc
      else
        origin match {
          case Empty        => acc
          case Cons(hd, tl) => go(remain - 1, Cons(hd, () => acc), tl())
        }
    go(n, Empty, this)
  }

  override def drop(n: Int): Stream[A] =
    if (n <= 0) this
    else
      this match {
        case Cons(_, tl) if n > 1  => tl().drop(n - 1)
        case Cons(_, tl) if n == 1 => tl()

      }

  override def takeWhile(p: A => Boolean): Stream[A] =
    if (p(h())) Cons(h, () => t().takeWhile(p)) else t().takeWhile(p)

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

    println(myStream.take(3).toListWithTailRec)
    println(myStream.take(10).toListWithTailRec)
    println(myStream.drop(4).toListWithTailRec)
    println(myStream.drop(1).toListWithTailRec)
    println(myStream.drop(12).toListWithTailRec)
    println(myStream.drop(0).toListWithTailRec)
    println(myStream.takeWhile(e => e % 2 == 0).toListWithTailRec)
    println(myStream.takeWhile(e => e + 1 > 4).toListWithTailRec)
    println(myStream.takeWhile(e => e + 1 > 10))

  }
}
