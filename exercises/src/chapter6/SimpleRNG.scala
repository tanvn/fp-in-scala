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

  override def nextStr: (String, RNG) = {
    val (num, rng) = nextInt
    (s"$num v", rng)
  }
}

object SimpleRNG {

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, r) = rng.nextInt
    (if (i < 0) -(i + 1) else i, r)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    (1 to count).foldRight(List.empty[Int], rng)(
      (_: Int, a: (List[Int], RNG)) => {
        val (nextInt, nextRNG) = a._2.nextInt
        (a._1 ::: nextInt :: Nil, nextRNG)
      }
    )

  }
  type Rand[+A] = RNG => (A, RNG)
  val intRand: Rand[Int] = _.nextInt
  // intRand is equivalent to the below (just a sugar syntax of scala using underscore)
  val intRand2: Rand[Int] = (rng) => rng.nextInt

  val stringRand: Rand[String] = _.nextStr

  val intRand3: Rand[Int] = (rng) => {
    val (n, rng2) = rng.nextInt
    (Math.abs(n - 5555), rng2)
  }

  //  val doubleRand : Rand[Double] = (rng) => {
  //    rng.nextInt * 1.5
  //  }

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    (rng: RNG) => {
      val (a, rng2: RNG) = s(rng)
      (f(a), rng2)
    }

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    (rng: RNG) => {
      //    val a3 : A = _
      //    val res: (A, RNG) = ra(a3)
      val (newA, rngA2: RNG) = ra(rng)
      val (newB, rngB2: RNG) = rb(rngA2)

      (f(newA, newB), rngB2)

    }

  def sequence2[A](fs: List[Rand[A]]): Rand[List[A]] =
//    fs.foldRight(unit(List[A]()))((f, acc: Rand[List[A]]) => map2(f, acc)(_ :: _))
    fs.foldRight(unit(List[A]()))(
      (f, acc: Rand[List[A]]) => map2(f, acc)((a: A, b: List[A]) => a :: b)
    )

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = rng => {
    fs.foldLeft(List.empty[A], rng)(
      (current: (List[A], RNG), next: Rand[A]) => {
        val (nextVal, nextRng) = next(current._2)
        (current._1 ::: nextVal :: Nil, nextRng)
      }
    )
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
    val seed = SimpleRNG(2000)
    println(ints(10)(seed))
    println(map(intRand)(i => s"value $i")(seed))

    val map2Test = map2(intRand, stringRand)((a, b) => s"$a vs $b")
    var newSeed: RNG = seed
    for (_ <- 1 to 10) {

      var res = map2Test(newSeed)
      println(res._1)
      newSeed = res._2

    }

    val seq2: Rand[List[Int]] = sequence2(
      List(_.nextInt, _.nonNegativeInt, intRand3)
    )
    println(seq2(seed))

    val seqMe: Rand[List[Int]] = sequence(
      List(_.nextInt, _.nonNegativeInt, intRand3)
    )
    println(seqMe(seed))

  }
}
