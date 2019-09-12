package chapter8

import chapter5.Stream
import chapter6.RNG
import chapter8.Prop.TestCases
import chapter8.PropWithMax.MaxSize

case class PropWithMax(run: (MaxSize, TestCases, RNG) => Result) {

  private def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(s => Some(g.sample.run(s)))

  private def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
      s"generated an exception: ${e.getMessage}\n" +
      s"stack trace: \n ${e.getStackTrace.mkString("\n")}"

  def &&(p: PropWithMax) = PropWithMax { (max, n, rng) =>
    run(max, n, rng) match {
      case Passed => p.run(max, n, rng)
      case x      => x
    }
  }

  def ||(p: PropWithMax) = PropWithMax { (max, n, rng) =>
    run(max, n, rng) match {
      // In case of failure, run the other prop.
      case Falsified(msg, _) => p.tag(msg).run(max, n, rng)
      case x                 => x
    }
  }

  /* This is rather simplistic - in the event of failure, we simply prepend
   * the given message on a newline in front of the existing message.
   */
  def tag(msg: String) = PropWithMax { (max, n, rng) =>
    run(max, n, rng) match {
      case Falsified(e, c) => Falsified(msg + "\n" + e, c)
      case x               => x
    }
  }

  def forAll[A](as: Gen[A])(f: A => Boolean): PropWithMax =
    PropWithMax((_, n, rng) => {
      var i = 0
      randomStream(as)(rng)
        .take(n)
        .map(a => {
          try {
            if (f(a)) Passed
            else {
              val res = Falsified(a.toString, i)
              i = i + 1
              res
            }
          } catch { case e: Exception => Falsified(buildMsg(a, e), i) }
        })
        .find(_.isFalsified)
        .getOrElse(Passed)
    })

  def forAll2[A](g: Int => Gen[A])(f: A => Boolean): PropWithMax = PropWithMax {
    (max: MaxSize, n: TestCases, rng: RNG) =>
      {
        val casesPerSize: Int = (n + (max - 1)) / max
        val props: Stream[PropWithMax] =
          Stream.from(0).take(n.min(max) + 1).map(i => forAll(g(i))(f))
        val prop: PropWithMax = props
          .map(
            p =>
              PropWithMax { (max, _, rng) =>
                {
                  p.run(max, casesPerSize, rng)
                }
              }
          )
          .toList
          .reduce(_ && _)
        prop.run(max, n, rng)
      }

  }

  def forAll[A](g: SGen[A])(f: A => Boolean): PropWithMax =
    forAll2(size => g.forSize(size))(f)

}

object PropWithMax {
  type MaxSize = Int

}
