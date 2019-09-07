package chapter6

case class SimpleRNG(seed: Long) extends RandomGenerator {
  override def nextInt: (Int, RandomGenerator) = {
    val newSeed = (seed * 0X5DEECE66DL + 0XBL) & 0XFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }

  override def nonNegativeInt: (Int, RandomGenerator) = {
    val (n, nextRNG) = nextInt
    if (n > Int.MinValue) (Math.abs(n), nextRNG)
    else nonNegativeInt
  }

  override def nextStr: (String, RandomGenerator) = {
    val (num, rng) = nextInt
    (s"$num v", rng)
  }
}

object SimpleRNG {

  def nonNegativeInt(rng: RandomGenerator): (Int, RandomGenerator) = {
    val (i, r) = rng.nextInt
    (if (i < 0) -(i + 1) else i, r)
  }

  def ints(count: Int)(rng: RandomGenerator): (List[Int], RandomGenerator) = {
    (1 to count).foldRight(List.empty[Int], rng)(
      (_: Int, a: (List[Int], RandomGenerator)) => {
        val (nextInt, nextRNG) = a._2.nextInt
        (a._1 ::: nextInt :: Nil, nextRNG)
      }
    )

  }
  type Rand[+A] = RandomGenerator => (A, RandomGenerator)
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

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    (rng: RandomGenerator) => {
      //    val a3 : A = _
      //    val res: (A, RNG) = ra(a3)
      val (newA, rngA2: RandomGenerator) = ra(rng)
      val (newB, rngB2: RandomGenerator) = rb(rngA2)

      (f(newA, newB), rngB2)

    }

  //  Explain how sequence2 works
  //  Input of List[Rand[A] as : List(f1, f2, f3)
//
//  init (List.empty) : Rand[List[A]] = (unitRNG) => (List.Empty, unitRNG)
//  Fold from right, so :
//  start with f3 and init
//    rngF3 = map2(f3, init) {
//    (rng) => {
//      a : A3, rngNew = f3(rng)
//      b : List[A], rngNew2 = init(rngNew)
//      //b is Empty, rngNew2 = rngNew is next state of rng  (1 transition)
//      (A3::Nil, rngNew )	// rngNew is 1 step transition from rng
//    }
//
//  }
//
//  next with f2 and the above result rngF3
//
//    rngF2 = map2(f2, rngF3 =(rngInput) => (A3::Nil, nextStateOfRng)) {
//    (rng) => {
//      a : A2, rngNew = f2(rng) // state transition 1
//      b : List[A](A3), rngNew2 = rngF3(rngNew) // state transition : 1 (explained above)
//
//      (A2::A3::Nil, rngNew2) //rngNew2 is 2 step transition from rng
//    }
//
//  }
//
//  next with f2 and the above result rngF2
//
//    rngF1 = map2(f1, rngF2 =(rngInput) => (A2::A3::Nil, nextStateOfRng)) {
//    (rng) => {
//      a : A1, rngNew = f1(rng) //state transition : 1
//      b : List[A](A2, A3), rngNew2 = rngF2(rngNew) //state transitions : 2  (explained above)
//      (A1::A2::A3::Nil, rngNew2) //rngNew2 is 3 (2 + 1) step transition from rng
//    }
//
//  }
//  rngF1 is returned, which is (rng) => (A1::A2::A3::Nil, rngNew2) with rngNew2 is 3 times transitions

  def sequence2[A](fs: List[Rand[A]]): Rand[List[A]] =
//    fs.foldRight(unit(List[A]()))((f, acc: Rand[List[A]]) => map2(f, acc)(_ :: _))
    fs.foldRight(unit(List[A]()))(
      (f, acc: Rand[List[A]]) => map2(f, acc)((a: A, b: List[A]) => a :: b)
    )

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = rng => {
    fs.foldLeft(List.empty[A], rng)(
      (current: (List[A], RandomGenerator), next: Rand[A]) => {
        val (nextVal, nextRng) = next(current._2)
        (current._1 ::: nextVal :: Nil, nextRng)
      }
    )
  }

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
    val (a, rngA) = f(rng)
    val res: Rand[B] = g(a)
    //call res with next state of rng
    res(rngA)
  }

  // return f(a) and next state of rng
  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    (rng: RandomGenerator) => {
      val (a, rng2: RandomGenerator) = s(rng)
      (f(a), rng2)
    }

  def mapWithFlatMap[A, B](s: Rand[A])(f: A => B): Rand[B] =
    // rng => (f(a), rng) return f(a) and rng (unchanged)
    flatMap(s)(a => rng => (f(a), rng))

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
    println(mapWithFlatMap(intRand)(i => s"value $i")(seed))

    val map2Test = map2(intRand, stringRand)((a, b) => s"$a vs $b")
    var newSeed: RandomGenerator = seed
    for (_ <- 1 to 10) {
      val res = map2Test(newSeed)
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
