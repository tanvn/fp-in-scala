package chapter10

object ExampleRunner {
  import Monoid._

  def main(args: Array[String]): Unit = {
    val words = List("hello", "world", "I", "am", "fine")
    val concatStr = words.foldRight(stringConcatMonoid.zero)(stringConcatMonoid.op)
    println(concatStr)
    val concatStr2 = words.foldRight(stringConcatMonoid.zero)(stringConcatMonoid.op)
    println(concatStr2)

  }

}
