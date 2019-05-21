package chapter5

sealed trait Stream[+A] {
  def toList: List[A]
  def toListWithTailRec: List[A]

  def take(n: Int): Stream[A]
  def drop(n: Int): Stream[A]

  def takeWhile(p: A => Boolean): Stream[A]
  def filter(p: A => Boolean): Stream[A]

  def foldRight[B](z: => B)(f: (A, => B) => B): B
  def exists(p: A => Boolean): Boolean
  def forAll(p: A => Boolean): Boolean

  def headOption: Option[A]

  def map[B](f: A => B): Stream[B]

  def append[B >: A](s: => Stream[B]): Stream[B]
}

case object Empty extends Stream[Nothing] {
  override def toList: List[Nothing] = List.empty[Nothing]
  def toListWithTailRec: List[Nothing] = List.empty[Nothing]

  override def take(n: Int): Stream[Nothing] = Empty

  override def drop(n: Int): Stream[Nothing] = Empty

  override def takeWhile(p: Nothing => Boolean): Stream[Nothing] = Empty

  override def foldRight[B](z: => B)(f: (Nothing, => B) => B): B = z

  override def exists(p: Nothing => Boolean): Boolean = false

  override def forAll(p: Nothing => Boolean): Boolean = true

  override def filter(p: Nothing => Boolean): Stream[Nothing] = Empty

  override def headOption: Option[Nothing] = None

  override def map[B](f: Nothing => B): Stream[B] = Empty

  override def append[B >: Nothing](s: => Stream[B]): Stream[B] = s
}

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A] {
  override def toList: List[A] = List(h()) ++ t().toList

  override def toListWithTailRec: List[A] = {
    println(s"Start convert Cons to a List")

    @annotation.tailrec
    def go(s: Stream[A], acc: List[A]): List[A] = s match {
      case Cons(h, t) => {
        println(s"go with tail, and add ${h()} to list")
        go(t(), acc :+ h())
      }
      case Empty => {
        println("List is empty, return the accumulated list")
        acc
      }
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

  def takeWhileOld(p: A => Boolean): Stream[A] =
    if (p(h())) Cons(h, () => t().takeWhile(p)) else t().takeWhile(p)

  override def takeWhile(p: A => Boolean): Stream[A] =
    foldRight(Stream.empty[A])((a, b) => {
      println("executing f of foldRight")
      if (p(a)) {
        //here a really lazy Cons is created, since b is unevaluated when passing to the function f of foldRight,
        // here it will stay unevaluated because it is wrapped into a function () => b.
        // Its value will be evaluated only when we convert the stream into a list, that's why I add many comments to track the flow :D
        println(s"return a lazy Cons with head is ${a}")
        Cons(() => a, () => {
          b
        })
      } else {
        println("return Empty of takeWhile")
        // if instead of returning Stream.empty[A] (as below) with b,
        // we will have a filter (take all the elements that satisfy p)
        //see filter function below
        Stream.empty[A]
      }
    })

  override def filter(p: A => Boolean): Stream[A] =
    foldRight(Stream.empty[A])((a, b) => {
      if (p(a)) {
        Cons(() => a, () => {
          b
        })
      } else {
        b
      }
    })

  override def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(hd, tl) => {
      println(s"foldRight head is ${hd()}")
      f(hd(), tl().foldRight(z)(f))
    }
    case _ => z
  }

  override def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

  def forAllOld(p: A => Boolean): Boolean = this match {
    case Cons(hd, tl) => {
      //println(s"head is ${hd()}")
      p(hd()) && tl().forAll(p)
    }
    case _ => false
  }
  override def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  override def headOption: Option[A] =
    foldRight(Option.empty[A])((a, _) => Some(a))

  override def map[B](f: A => B): Stream[B] =
    foldRight(Stream.empty[B])((a: A, b) => Cons(() => f(a), () => b))

  override def append[B >: A](s: => Stream[B]): Stream[B] =
    foldRight(s)((a, b) => Cons(() => a, () => b))
}

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def constant[A](a: A): Stream[A] = {
    lazy val src: Stream[A] = Stream.cons(a, src)
    src
  }

  def from(n: Int): Stream[Int] = Stream.cons(n, from(n + 1))

  def numbersWithStep(from: Int, step: Int): Stream[Int] =
    Stream.cons(from, numbersWithStep(from + step, step))

  def fibs: Stream[Int] = {
    def go(prev1: Int, prev2: Int): Stream[Int] =
      Stream.cons(prev1, go(prev2, prev2 + prev1))
    go(0, 1)
  }

  def fibsFromUnFold: Stream[Int] = {
    unfold((0, 1))(state => Some(state._1, (state._2, state._1 + state._2)))
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
      case None         => Stream.empty[A]
      case Some((a, s)) => Stream.cons(a, unfold(s)(f))
    }

  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty[A] else cons(as.head, apply(as.tail: _*))

  def main(args: Array[String]): Unit = {

    val myStream = Stream(1, 2, 3, 4, 5, 6, 7)
//    println(myStream.toList)
//    println(myStream.toListWithTailRec)
//
//    println(myStream.take(3).toListWithTailRec)
//    println(myStream.take(10).toListWithTailRec)
//    println(myStream.drop(4).toListWithTailRec)
//    println(myStream.drop(1).toListWithTailRec)
//    println(myStream.drop(12).toListWithTailRec)
//    println(myStream.drop(0).toListWithTailRec)

    println("takeWhile start")
    println(myStream.takeWhile(e => e <= 2).toListWithTailRec)
    println(myStream.takeWhile(e => e % 2 == 0).toListWithTailRec)
    println(myStream.takeWhile(e => e + 1 > 10))

    println("check exist")
    println(myStream.exists(a => a % 3 == 0))
    println(myStream.exists(a => a % 6 == 0))

    println("forAll check")
    println(myStream.forAll(a => a % 3 == 0))
    println(myStream.forAll(a => a >= 0))
    println("filter check")
    println(myStream.filter(a => a % 3 == 0).toListWithTailRec)
//    println(myStream.filter(a => a % 2 == 0).toListWithTailRec)
    println(myStream.headOption)
    println(myStream.map(a => a * 2).toListWithTailRec)

    println("append check")
    println(myStream.append(Stream(10, 11, 12)).toListWithTailRec)

    println("map then filter check")
    println(Stream(1, 2, 3, 4).map(_ + 10).filter(_ % 2 == 0).toListWithTailRec)

    lazy val ones: Stream[Int] = Stream.cons(1, ones)
    // println(ones.takeWhile(_ == 1).toListWithTailRec) //infinitely run
    println(ones.take(10).toListWithTailRec)

    println(Stream.constant(10).take(5).toListWithTailRec)
    println(Stream.from(10).take(10).toListWithTailRec)
    println(Stream.numbersWithStep(10, 2).take(10).toListWithTailRec)
    println(Stream.fibs.take(10).toListWithTailRec)

    val fibs = {
      def go(f0: Int, f1: Int): Stream[Int] =
        cons(f0, go(f1, f0 + f1))
      go(0, 1)
    }

    println(fibs.take(10).toListWithTailRec)
    val unfoldInt = unfold(1)(a => Some(a, 10 - a))
    println(unfoldInt.take(10).toListWithTailRec)

    println(fibsFromUnFold.take(10).toListWithTailRec)
  }
}
