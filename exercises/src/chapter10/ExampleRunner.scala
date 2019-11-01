package chapter10

object ExampleRunner {
  import Monoid._

  def f(a : Int, b : Int)(f : Int => Int) : Int = f(a+b)

  def concatenate[A](as: List[A], m: Monoid[A]): A =
    as.foldLeft(m.zero)(m.op)

  // Notice that this function does not require the use of `map` at all.
  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    as.foldLeft(m.zero)((b, a) => m.op(b, f(a)))

  //by tanvn
  def foldMapOrigin[A,B](as: List[A], m: Monoid[B])(f: A => B): B = as.map(f).foldLeft(m.zero)(m.op)

  val test1: (Int => String => String) => String => String = foldMap(List.empty[Int], endoMonoid[String])
  val test2: (Double => String) => String = foldMap(List.empty[Double], stringConcatMonoid)


  // here foldMap(as, endoMonoid[B]) returns a function f = (A => B => B ) => (B => B)
  // because endoMonoid[B] : Monoid[B => B]

  // zer
  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
    foldMap(as, endoMonoid[B])(f.curried)(z)


  def main(args: Array[String]): Unit = {
    val words = List("hello", "world", "I", "am", "fine")
    val concatStr = words.foldRight(stringConcatMonoid.zero)(stringConcatMonoid.op)
    println(concatStr)
    val concatStr2 = words.foldRight(stringConcatMonoid.zero)(stringConcatMonoid.op)
    println(concatStr2)

  }

}
