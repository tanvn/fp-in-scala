package chapter6

case class SimpleRNG(seed: Long) extends RNG {
  override def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }

  override def nonNegativeInt: (Int, RNG) = {
    val (n, nextRNG) = nextInt
    if (n > Int.MinValue) (Math.abs(n), nextRNG)
    else nonNegativeInt
  }
}

object SimpleRNG {

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, r) = rng.nextInt
    (if (i < 0) -(i + 1) else i, r)
  }

  def main(args: Array[String]): Unit = {
    val (n, nextRNG) = SimpleRNG(42).nextInt
    println(n)
    val (n2, nextRNG2) = nextRNG.nextInt
    println(n2)
    val (n3, nextRNG3) = nextRNG2.nextInt
    println(n3)

    val (n4, nextRNG4) = nextRNG3.nonNegativeInt

    println(n4)

  }
}
