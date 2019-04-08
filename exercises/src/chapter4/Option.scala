package chapter4

sealed trait Option[+A] {

  def map[B](f: A => B): Option[B] = this match {
    case None    => None
    case Some(a) => Some(f(a))
  }
  def getOrElse[B >: A](default: => B): B = this match {
    case None    => default
    case Some(a) => a
  }
  def filter(f: A => Boolean): Option[A] = flatMap { a: A =>
    if (f(a)) Some(a) else None
  }

  def flatMap[B](f: A => Option[B]): Option[B] = map(f) getOrElse None

  def filter2(f: A => Boolean): Option[A] = this match {
    case None    => None
    case Some(a) => if (f(a)) Some(a) else None
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = map(Some(_)) getOrElse ob

  def meanInt(a: Seq[Int]): Option[Double] =
    if (a.isEmpty) None else Some(a.sum / (a.length * 1.0))
  def mean(a: Seq[Double]): Option[Double] =
    if (a.isEmpty) None else Some(a.sum / (a.length * 1.0))

  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs) flatMap (m => mean(xs.map(x => math.pow(x - m, 2))))



}
case class Some[+A](get: A) extends Option[A]

case object None extends Option[Nothing]

object Option {

  def lift[A,B](f : A => B) : Option[A] => Option[B] = _ map f

  val absO: Option[Double] => Option[Double] = lift(math.abs)


  def variance(xs: Seq[Double]): Option[Double] =
    Some(xs).flatMap(xs => mean(xs)).map { mean =>
      xs.foldLeft(0.0)((res, a) => res + Math.pow(a - mean, 2)) / xs.length
    }
  def mean(a: Seq[Double]): Option[Double] =
    if (a.isEmpty) None else Some(a.sum / (a.length * 1.0))

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = (a, b) match {
    case (Some(aS), Some(bS)) => Some(f(aS,bS))
    case _ => None
  }

  def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
    case Some(h) :: tail => sequence(tail).map(list => {
      h :: list
    })
    case None:: _ =>
      None
    case Nil =>
      Some(List.empty[A])

  }
  def main(args: Array[String]): Unit = {
    val a = Some(10)
    println(a.map(_ * 2))

    val b = a.flatMap { a =>
      if (a % 2 == 0) Some(a / 2) else None
    }
    println(b)

    val c = a.map(_ * 2 + 1).flatMap { a =>
      if (a % 2 == 0) Some(a / 2) else None
    }
    println(c)
    println(variance(Seq(1,2,3)))
    println(variance(Seq.empty))

    println(Some(15).filter(_ % 4 ==0))
    println(Some(15).filter(_ % 5 ==0))

    println(sequence(List(Some(1), Some(2), Some(3), Some(4))))
    println(sequence(List(None)))
    println(sequence(List(None, Some(1), Some(2))))
    println(sequence(List(Some(10), Some(11),None, Some(1), Some(2))))


  }





}