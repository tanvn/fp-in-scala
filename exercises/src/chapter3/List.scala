package chapter3

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil else Cons(as.head, apply(as.tail: _*))

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def tail[A](a: List[A]): List[A] = a match {
    case Cons(_, xs) => xs
    case Nil => Nil
  }

  def drop[A](l: List[A], n: Int): List[A] = {
    if (n == 1) {
      tail(l)
    } else {
      drop(tail(l), n - 1)
    }
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
    l match {
      case Cons(h, t) => if (f(h)) dropWhile(t, f) else l
      case Nil => Nil
    }

  def init[A](l: List[A]): List[A] = {
    l match {
      case Cons(_, Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))
    }
  }


  def setHead[A](a: List[A], head: A): List[A] = a match {
    case Nil => Nil
    case Cons(_, xs) => Cons(head, xs)
  }


  def foldLeftStraight[A, B](as: List[A], base: B, f: (A, B) => B): B = {
    as match {
      case Nil => base
      case Cons(h, t) => foldLeftStraight(t, f(h, base), f)
    }
  }

  def foldLeft[A, B](as: List[A], b: B)(f: (A, B) => B): B = {
    as match {
      case Nil => b
      case Cons(h, t) => foldLeft(t, f(h, b))(f)
    }
  }

  def foldRight[A, B](as: List[A], b: B)(f: (A, B) => B): B = {
    as match {
      case Nil => b
      case Cons(h, t) => f(h, foldRight(t, b)(f))
    }
  }

  def main(args: Array[String]): Unit = {
    val a = List(1, 2, 3, 4, 5, 6, 7)
    println(drop(a, 2))

    println(init(a))
    println(dropWhile(a, (a: Int) => a >= 1 && a < 5))
    val res = a match {
      case foo => 42

    }
    println(res)

    val x = List(1, 2, 3, 4, 5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t) => h + sum(t)
      case _ => 101
    }

    println(x)

    val foldLeftProduct = foldLeftStraight(List(1, 3, 5, 7), 1, (a: Int, b: Int) => a * b)
    println(s"product is ${foldLeftProduct}")

    val foldLeftSum = foldLeft(List(1, 2, 3, 4, 5, 6, 7, 8), 0)((a: Int, b: Int) => a + b)

    println(s"foldLeftSum is $foldLeftSum")

    val foldRightSum = foldRight(List(1, 2, 3, 4, 5, 6, 7, 8), 0)((a: Int, b: Int) => a + b)
    println(s"foldRightSum is $foldRightSum")


  }
}
