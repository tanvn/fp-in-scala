package chapter4

sealed trait Partial[+A,+B] {

  def map[C]( f: B => C) : Partial[A, C] = this match {
    case Errors(e) => Errors(e)
    case Success(v) => Success(f(v))

  }

  def map2[EE >: A,C,D](b: => Partial[EE, C])( f: (B, C) => D) : Either[EE, D] = ???
}

case class Errors[+A](get: Seq[A]) extends Partial[A,Nothing]
case class Success[+B](get: B) extends Partial[Nothing,B]

object Partial {

  def traverse[E, A, B](as: List[A])(
    f: A => Partial[E, B]): Partial[E, List[B]] =  {

  def traverseWithErr(as: List[A], error : Seq[E], res : List[B])(
    f: A => Partial[E, B]) : Partial[E, List[B]] = as match {
    case Nil => if(error.isEmpty) Success(res) else Errors(error)
    case h::tail => f(h) match {
      case Errors(e) => traverseWithErr(tail, error ++ e, res)(f)
      case Success(v) => traverseWithErr(tail, error, res++List(v))(f)
    }
  }
    traverseWithErr(as, List.empty, List.empty)(f)
  }

  def main(args: Array[String]): Unit = {

    val testData1 = List(19,29,39,40,41, 60, 32)

    println(traverse(testData1)(a => if(a % 2 == 0) Success(a/2) else Errors(Seq(s" $a is not an even number"))))
  }
}
