package chapter3

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil else Cons(as.head, apply(as.tail: _*))

  def sum(ints: List[Int]): Int = ints match {
    case Nil         => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil          => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs)  => x * product(xs)
  }

  def tail[A](a: List[A]): List[A] = a match {
    case Cons(_, xs) => xs
    case Nil         => Nil
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
      case Nil        => Nil
    }

  def init[A](l: List[A]): List[A] = {
    l match {
      case Cons(_, Nil) => Nil
      case Cons(h, t)   => Cons(h, init(t))
    }
  }

  def setHead[A](a: List[A], head: A): List[A] = a match {
    case Nil         => Nil
    case Cons(_, xs) => Cons(head, xs)
  }

  def foldLeftStraight[A, B](as: List[A], base: B, f: (A, B) => B): B = {
    as match {
      case Nil        => base
      case Cons(h, t) => foldLeftStraight(t, f(h, base), f)
    }
  }

  def foldLeft[A, B](as: List[A], b: B)(f: (A, B) => B): B = {
    as match {
      case Nil        => b
      case Cons(h, t) => foldLeft(t, f(h, b))(f)
    }
  }

  def foldRightViaFoldLeft_1[A, B](l: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(l, (b: B) => b)((a: A, g: B => B) => (b: B) => g(f(a, b)))(z)

  def foldRight[A, B](as: List[A], b: B)(f: (A, B) => B): B = {
    as match {
      case Nil        => b
      case Cons(h, t) => f(h, foldRight(t, b)(f))
    }
  }

  def foldLeftFromFoldRight[A, B](as: List[A], b: B)(f: (A, B) => B): B =
    foldRight(reverse(as), b)(f)

  def length[A](as: List[A]): Int = foldRight(as, 0)((_: A, b: Int) => b + 1)

  def reverse[A](as: List[A]): List[A] =
    foldLeft(as, Nil: List[A])((next, b) => Cons(next, b))

  def appendA[A](as: List[A], a: A): List[A] =
    foldRight(as, Cons(a, Nil))((e, l) => Cons(e, l))

  def append[A](as: List[A], t: List[A]): List[A] =
    foldRight(as, t)((e, l) => Cons(e, l))

  def concat[A](l: List[List[A]]): List[A] =
    foldRight(l, Nil: List[A])(append)

  def add1(as: List[Int]): List[Int] =
    foldRight(as, Nil: List[Int])((a, l) => Cons(a + 1, l))

  def toString(as: List[Double]): List[String] =
    foldRight(as, Nil: List[String])((a, l) => Cons(a.toString, l))

  def map[A, B](as: List[A])(f: A => B): List[B] = as match {
    case Nil        => Nil
    case Cons(h, t) => Cons(f(h), map(t)(f))
  }

  def filter[A](as: List[A])(f: A => Boolean): List[A] = as match {
    case Nil        => Nil
    case Cons(h, t) => if (f(h)) Cons(h, filter(t)(f)) else filter(t)(f)
  }

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = as match {
    case Nil        => Nil
    case Cons(h, t) => append(f(h), flatMap(t)(f))
  }

  def filterViaFlatMap[A](l: List[A])(f: A => Boolean): List[A] =
    flatMap(l)(a => if (f(a)) List(a) else Nil)

  def add2Lists(a: List[Int], b: List[Int]): List[Int] = (a, b) match {
    case (Nil, _)                     => Nil
    case (_, Nil)                     => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, add2Lists(t1, t2))
  }

  def zipWith[A, B](a: List[A], b: List[A])(f: (A, A) => B): List[B] =
    (a, b) match {
      case (Nil, _)                     => Nil
      case (_, Nil)                     => Nil
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))

    }

  def main(args: Array[String]): Unit = {
    val a = List(1, 2, 3, 4, 5, 6, 7)
    println(drop(a, 2))

    println(init(a))
    println(dropWhile(a, (a: Int) => a >= 1 && a < 5))
    val resM = a match {
      case foo => 42

    }
    println(resM)

    val x = List(1, 2, 3, 4, 5) match {
      case Cons(x, Cons(2, Cons(4, _)))          => x
      case Nil                                   => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t)                            => h + sum(t)
      case _                                     => 101
    }

    println(x)

    val foldLeftProduct =
      foldLeftStraight(List(1, 3, 5, 7), 1, (a: Int, b: Int) => a * b)
    println(s"product is ${foldLeftProduct}")

    val foldLeftSum =
      foldLeft(List(1, 2, 3, 4, 5, 6, 7, 8), 0)((a: Int, b: Int) => a + b)
    val foldLeftSumFromFoldRight =
      foldLeftFromFoldRight(List(1, 2, 3, 4, 5, 6, 7, 8), 0)(
        (a: Int, b: Int) => a + b
      )

    println(s"foldLeftSum is $foldLeftSum")
    println(s"foldLeftSumFromFoldRight is $foldLeftSumFromFoldRight")

    val foldRightSum =
      foldRight(List(1, 2, 3, 4, 5, 6, 7, 8), 0)((a: Int, b: Int) => a + b)
    println(s"foldRightSum is $foldRightSum")
    val res = foldRight(List(1, 2, 3), Nil: List[Int])(Cons(_, _))

    println(res)
    println(length(List(1, 2, 3, 4, 5)))
    println(length(Nil))

    println(reverse(List(1, 2, 3, 4, 5)))
    println(appendA(List(1, 2, 3, 4), 5))

    val res2 =
      foldRightViaFoldLeft_1(List(1, 2, 3, 4), 0)((a: Int, b: Int) => a + b)
    println(res2)

    //((a, g) => b => g(f(a,b)))(z)
    val g = (b: Int) => b * 3
    val f = (a: Int, b: Int) => a + b
    val test =
      ((a: Int, b: (Int => Int)) => ((c: Int) => g(f(a, c))))(10, b => b)
    println(test)
    println(test(15))

    println(add1(List(1, 3, 4, 5, 7, 6)))
    println(map(List(1, 2, 3, 4, 5, 6))(a => a * 2))
    println(filter(List(1, 2, 3, 4, 5, 6))(a => a % 2 == 0))
    println(flatMap(List(1, 2, 3, 4))(a => List(a, a + 1)))
    println(filterViaFlatMap(List(1, 2, 3, 4, 5, 6))(a => a % 2 == 0))
    println(add2Lists(List(1, 2, 3, 4), List(4, 5, 6)))
  }
}
