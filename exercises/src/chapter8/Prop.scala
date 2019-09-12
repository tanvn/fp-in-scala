package chapter8

import chapter5.Stream
import chapter6.RNG
import chapter8.Prop.TestCases

//trait Prop {
//  def check: Either[(FailedCase, SuccessCount), SuccessCount]
//
////  def &&(p: Prop): Prop = new Prop {
////    def check: Boolean = this.check && p.check
////  }
//}

object Prop {
  type FailedCase = String
  type SuccessCount = Int
  type TestCases = Int

  private def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(s => Some(g.sample.run(s)))

  private def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
      s"generated an exception: ${e.getMessage}\n" +
      s"stack trace: \n ${e.getStackTrace.mkString("\n")}"

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop =
    Prop((n, rng) => {
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

}

case class Prop(run: (TestCases, RNG) => Result) {

  def &&(p: Prop) = Prop { (n, rng) =>
    run(n, rng) match {
      case Passed => p.run(n, rng)
      case x      => x
    }
  }

  def ||(p: Prop) = Prop { (n, rng) =>
    run(n, rng) match {
      // In case of failure, run the other prop.
      case Falsified(msg, _) => p.tag(msg).run(n, rng)
      case x                 => x
    }
  }

  /* This is rather simplistic - in the event of failure, we simply prepend
   * the given message on a newline in front of the existing message.
   */
  def tag(msg: String) = Prop { (n, rng) =>
    run(n, rng) match {
      case Falsified(e, c) => Falsified(msg + "\n" + e, c)
      case x               => x
    }
  }
}
