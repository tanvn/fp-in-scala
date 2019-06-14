package chapter6

trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.

  def nextStr: (String, RNG)
}

case class Simple(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
    val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
    val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
    (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
  }

  override def nextStr: (String, RNG) = {
    val (num, rng) = nextInt
    (s"$num v", rng)
  }
}




object Simple {

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
     (1 to count).foldRight(List.empty[Int], rng)((_ : Int, a : (List[Int], RNG)) => {
      val (nextInt, nextRNG) = a._2.nextInt
      (a._1::: nextInt::Nil, nextRNG)
    })

  }
  type Rand[+A] = RNG => (A, RNG)
  val intRand: Rand[Int] = _.nextInt
  // intRand is equivalent to the below (just a sugar syntax of scala using underscore)
  val intRand2 : Rand[Int] = (rng) => rng.nextInt

  val stringRand : Rand[String] = _.nextStr

//  val doubleRand : Rand[Double] = (rng) => {
//    rng.nextInt * 1.5
//  }

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    (rng: RNG) => {
      val (a, rng2: RNG) = s(rng)
      (f(a), rng2)
    }


  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = (rng : RNG) => {
//    val a3 : A = _
//    val res: (A, RNG) = ra(a3)
    val (newA, rngA2: RNG) = ra(rng)
    val (newB, _: RNG) = rb(rng)


    (f(newA,newB),rngA2)

  }

  def main(args: Array[String]): Unit = {

    val seed = Simple(2000)
    println(ints(10)(seed))
    println(map(intRand)(i => s"value $i")(seed))

    val map2Test = map2(intRand,stringRand)((a,b) => s"$a vs $b")
    var newSeed: RNG = seed
    for( _ <- 1 to 10) {

      var res = map2Test(newSeed)
      println(res._1)
      newSeed = res._2

    }
  }

}
