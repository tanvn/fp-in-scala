package chapter8

import chapter6.{RNG, State}

case class Gen[A](sample: State[RNG, A]) {

  def flatMap[B](f: A => Gen[B]): Gen[B] = {
    Gen(State(s => {
      //get a generated a and the next state with the provide state
      val (a, s2: RNG) = sample.run(s)
      val (b, s3: RNG) = f(a).sample.run(s2)
      (b, s3)
    }))
  }

  def map[B](f: A => B): Gen[B] = {
    Gen(State(s => {
      //get a generated a and the next state with the provide state
      val (a, s2) = sample.run(s)
      (f(a), s2)
    }))
  }

  def map2[B](f: A => B): Gen[B] = flatMap(a => Gen.unit(f(a)))

  def listOfN(size: Gen[Int]): Gen[List[A]] =
    flatMap((a: A) => {
      Gen(State(s => {
        // get the size N of the List to generate
        val (n, s2) = size.sample.run(s)
        // fill it all with a
        (List.fill(n)(a), s2)
      }))
    })

// combining two generators of the same type into one, by pulling values from each generator with equal likelihood combining two generators of the same type into one, by pulling values from each generator with equal likelihood
  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = {
    Gen(State(s => {
      val (res, s2) = Gen.boolean.sample.run(s)
      if (res) {
        val (fromG1, s3) = g1.sample.run(s2)
        (fromG1, s3)
      } else {
        val (fromG2, s3) = g2.sample.run(s2)
        (fromG2, s3)
      }
    }))
  }

//accepts a weight for each Gen and generates values from each Gen with probability proportional to its weight.
  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
    Gen(State(s => {
      val (rand, s2) = Gen.choose(0, 101).sample.run(s)
      val g1Threshold = g1._2 / (g1._2 + g2._2) * 100
      if (rand <= g1Threshold) {
        g1._1.sample.run(s2)
      } else {
        g2._1.sample.run(s2)
      }
    }))
  }

}

object Gen {

  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive - start)))

  def unit[A](a: => A): Gen[A] = Gen(State.unit(a))

  def boolean: Gen[Boolean] = Gen(State(RNG.boolean))

  def double: Gen[Double] = Gen(State(RNG.double))

//  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = Gen(State(RNG.ints(n)))

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    Gen(State.sequence(List.fill(n)(g.sample)))

  def chooseInt2(start: Int, stopExclusive: Int): Gen[(Int, Int)] = {
    val genInt = choose(start, stopExclusive)
    Gen(State((s: RNG) => {
      val (int1, s1) = genInt.sample.run(s)
      val (int2, s2) = genInt.sample.run(s1)
      ((int1, int2), s2)
    }))
  }

  def toOpt[A](genA: Gen[A]): Gen[Option[A]] = {
    Gen(State(s => {
      val (a, s2) = genA.sample.run(s)
      (Some(a), s2)
    }))
  }

  def flatMap[A](opt: Gen[Option[A]]): Gen[A] = {
    Gen(State(s => {
      val (a, s2) = opt.sample.run(s)
      (a.getOrElse(null), s2)
    }))
  }

}
