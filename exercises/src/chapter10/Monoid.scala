package chapter10

trait Monoid[A] {
  def op(a1: A, a2: A): A

  def zero: A
}

object Monoid {

  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    override def op(a1: Option[A], a2: Option[A]): Option[A] = a1.orElse(a2)

    override def zero: Option[A] = None

  }

  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    override def op(a1: A => A, a2: A => A): A => A = a1.compose(a2)

    override def zero: A => A = a => identity(a)
  }

  val stringConcatMonoid = new Monoid[String] {
    override def op(a1: String, a2: String): String = a1 + a2

    override def zero: String = ""
  }

  val plusIntMonoid = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1 + a2

    override def zero: Int = 0
  }

  // for exercise 9
  val orderingIntMonoid = new Monoid[(IndexedSeq[Int], Boolean)] {
    override def op(a1: (IndexedSeq[Int], Boolean), a2: (IndexedSeq[Int], Boolean)): (IndexedSeq[Int], Boolean) = {
      if(a1._1.isEmpty) {
        a2
      } else if(a2._1.isEmpty) {
        a1
      }else {
        if(a1._1.last > a2._1.head) {
          (a2._1, false)
        } else {
          (a1._1 ++ a2._1, a1._2 && a2._2)
        }
      }
    }

    override def zero: (IndexedSeq[Int], Boolean) = (IndexedSeq.empty, true)
  }


}
