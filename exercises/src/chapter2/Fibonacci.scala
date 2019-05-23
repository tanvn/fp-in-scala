package chapter2

object Fibonacci {

  private val buf = scala.collection.mutable.ArrayBuffer.empty[Int]
  buf += 0
  buf += 1

  def fib(n: Int): Int = {

    @annotation.tailrec
    def go(n: Int, prev1: Int, prev2: Int): Int = {
      if (n == 0) prev1 + prev2
      else {
        go(n - 1, prev1 + prev2, prev1)
      }
    }
    if (n >= 2)
      go(n - 2, 1, 0)
    else n
  }

  def fibWithCache(n: Int): Int = {
    if (buf.nonEmpty && buf.length > n) {
      println(s"take from array, index= $n, val = ${buf(n)}")
      buf(n)
    } else {
      buf += (fibWithCache(n - 1) + fibWithCache(n - 2))
      println(s" $n just update buf to : $buf ")
      buf(n)
    }

  }

  def findFirst[A](as: Array[A], p: A => Boolean): Int = {
    @annotation.tailrec
    def loop(n: Int): Int = {
      if (n >= as.size) -1
      else if (p(as(n))) n
      else loop(n + 1)
    }
    loop(0)
  }

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {

    def loop(n: Int): Boolean = {
      if (n >= as.size - 1) true
      else if (ordered(as(n), as(n + 1))) loop(n + 1)
      else false
    }

    loop(0)

  }
  def partial1[A, B, C](a: A, f: (A, B) => C): B => C = (b: B) => f(a, b)

  def curry[A, B, C](f: (A, B) => C): A => (B => C) = (a: A) => { (b: B) =>
    f(a, b)
  }

  def uncurry[A, B, C](f: A => B => C): (A, B) => C = (a: A, b: B) => f(a)(b)

  def myCompose[A, B, C](f: B => C, g: A => B): A => C = (a: A) => f(g(a))

  def main(args: Array[String]): Unit = {
    println("Start")
    println(fib(5))
    println("End")

    val test = Array(1, 3, 4, 5, 9, 10, 34, 21)
    val first = findFirst(test, (ele: Int) => { ele % 2 == 0 })
    println(first)

    val sortedArr = Array(1, 2, 3, 4, 5)

    println(isSorted(test, (cur: Int, next: Int) => cur <= next))
    println(isSorted(sortedArr, (cur: Int, next: Int) => cur <= next))

    val c = curry((a: Int, b: Int) => s"$a and $b")
    val c2 = c(10)
    val str = c2(15)
    println(str)

    val p = partial1(5, (a: Int, b: Int) => s"$a and $b")
    println(p(15))

    val plus3 = (a: Int) => a + 3
    val string2Int = (b: String) => Integer.parseInt(b)

    val comFunc = myCompose(plus3, string2Int)

    val comFuncByKeyword = plus3 compose string2Int
    val comFuncByKeywor2 = string2Int andThen plus3

    println(comFunc("30"))
    println(comFuncByKeyword("14"))
    println(comFuncByKeyword("16"))
    println(fibWithCache(15))

    println(fibWithCache(10))
    println(fibWithCache(5))
  }

}
