package chapter10

object Excercise7 {
  import Monoid._
  import chapter7.Future._

  // This ability to 'lift' a monoid any monoid to operate within
  // some context (here `Par`) is something we'll discuss more in
  // chapters 11 & 12
  def par[A](m: Monoid[A]): Monoid[Par[A]] = new Monoid[Par[A]] {
    def zero: Par[A] = unit(m.zero)
    def op(a: Par[A], b: Par[A]): Par[A] = map2(a, b)(m.op)
  }

  def foldMapV[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {

    if (v.isEmpty) {
      m.zero
    } else if (v.size == 1) {
      m.op(m.zero, f(v.head))

    } else if (v.size == 2) {
      m.op(f(v.head), f(v(1)))
    } else {
      val (left, right) = v.splitAt(v.size / 2)
      println(s"left=$left, right= $right")
      m.op(foldMapV(left, m)(f), foldMapV(right, m)(f))
    }
  }

  def countWord(text: String): WC = {
    if (text.isEmpty) {
      Stub("")
    } else if (text.length == 1) {
      Stub(text)
    } else {
      val (left, right) = text.splitAt(text.length / 2)

      val res = wcMonoid.op(countWord(left), countWord(right))
//      println(s"left=${countWord(left)}, right=${countWord(right)}, res= $res")
      res

    }
  }

  def countWordToInt(text: String): Int =
    countWord(text) match {
      case Stub(chars) => if (chars.isEmpty) 0 else 1
      case Part(lStub, words, rStub) => {
        val lWord = if (lStub.isEmpty) 0 else 1
        val rWord = if (rStub.isEmpty) 0 else 1
        words + lWord + rWord
      }
    }

  def main(args: Array[String]): Unit = {
//    val data = IndexedSeq[Int](1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
//    val res = foldMapV(data, plusIntMonoid)(identity)
//    println(res)
//
//    val isDataOrdered = foldMapV(data, orderingIntMonoid)(i => (IndexedSeq(i), true))
//    println(isDataOrdered)
//
//    val data2 = IndexedSeq(1, 3, 5, 7, 6, 8)
//    val isDataOrdered2 = foldMapV(data2, orderingIntMonoid)(i => (IndexedSeq(i), true))
//    println(isDataOrdered2)
//
//    val data3 = IndexedSeq(1, 3, 5, 7, 8, 6)
//    val isDataOrdered3 = foldMapV(data3, orderingIntMonoid)(i => (IndexedSeq(i), true))
//    println(isDataOrdered3)
//
//    val data4 = IndexedSeq(1)
//    val isDataOrdered4 = foldMapV(data4, orderingIntMonoid)(i => (IndexedSeq(i), true))
//    println(isDataOrdered4)
//
//    val data5 = IndexedSeq.empty
//    val isDataOrdered5 = foldMapV(data5, orderingIntMonoid)(i => (IndexedSeq(i), true))
//    println(isDataOrdered5)

    val testText = "lorem ipsum dolor sit amet, "
//    val testText = "lorem ipsum dolor"
//    val testText = "ab cd e"
//    val cw1 = countWord(testText)
//    println(cw1)
//    println(countWordToInt(testText))

//    val testText2 = "hello world !, this is me, I am living in Tokyo"
//    println(countWord(testText2))
//
//    val testText3 =
//      "My \"starter project\" is out in Chrome Canary! There is still quite a lot of work both on our and tooling side to provide seamless WebAssembly debugging experience, but I'm very happy about each small step in that direction."
//    println(countWord(testText3))

    val testText4 =
      "  A   big part     of my job is creating an enabling, respectful, empowering, supportive culture to just get out of the way and let people do great work. We're #UCSFProud to see Chancellor Hawgood honored in the @SFBusinessTimes"
    println(countWord(testText4))
    println(countWordToInt(testText4))

//    val testText5 = "  A big part "
//    println(countWord(testText5))
//    println(countWordToInt(testText5))

  }

}
