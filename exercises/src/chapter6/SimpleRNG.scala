package chapter6

trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

case class Simple(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
    val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
    val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
    (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
  }
  
}


object Simple {

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
     (1 to count).foldRight(List.empty[Int], rng)((_ : Int, a : (List[Int], RNG)) => {
      val (nextInt, nextRNG) = a._2.nextInt
      (a._1::: nextInt::Nil, nextRNG)
    })

  }

  def main(args: Array[String]): Unit = {

    val seed = Simple(2000)
    println(ints(10)(seed))
  }

}
