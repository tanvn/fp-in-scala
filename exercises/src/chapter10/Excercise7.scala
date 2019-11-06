package chapter10

object Excercise7 {
  import Monoid._
  import chapter7.Future._

  // This ability to 'lift' a monoid any monoid to operate within
  // some context (here `Par`) is something we'll discuss more in
  // chapters 11 & 12
  def par[A](m: Monoid[A]): Monoid[Par[A]] = new Monoid[Par[A]] {
    def zero: Par[A] = unit(m.zero)
    def op(a: Par[A], b: Par[A]): Par[A] = map2(a,b)(m.op)
  }


  def foldMapV[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {

    if(v.isEmpty) {
      m.zero
    } else if(v.size ==1) {
      m.op(m.zero, f(v.head))

    }else if(v.size == 2) {
      m.op(f(v.head), f(v(1)))
    } else {
      val (left, right) = v.splitAt(v.size/2)
      println(s"left=$left, right= $right")
      m.op(foldMapV(left, m)(f), foldMapV(right,m)(f))
    }
  }

  def main(args: Array[String]): Unit = {
    val data = IndexedSeq[Int](1,2,3,4,5,6,7,8,9,10)
    val res = foldMapV(data, plusIntMonoid)(identity)
    println(res)

    val isDataOrdered= foldMapV(data, orderingIntMonoid)(i => (IndexedSeq(i), true))
    println(isDataOrdered)

    val data2 = IndexedSeq(1,3,5,7,6,8)
    val isDataOrdered2= foldMapV(data2, orderingIntMonoid)(i => (IndexedSeq(i), true))
    println(isDataOrdered2)

    val data3 = IndexedSeq(1,3,5,7,8, 6)
    val isDataOrdered3= foldMapV(data3, orderingIntMonoid)(i => (IndexedSeq(i), true))
    println(isDataOrdered3)

    val data4 = IndexedSeq(1)
    val isDataOrdered4= foldMapV(data4, orderingIntMonoid)(i => (IndexedSeq(i), true))
    println(isDataOrdered4)

    val data5 = IndexedSeq.empty
    val isDataOrdered5 = foldMapV(data5, orderingIntMonoid)(i => (IndexedSeq(i), true))
    println(isDataOrdered5)
  }


}
