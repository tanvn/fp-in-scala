package chapter4

sealed trait Option[+A] {

  def map[B](f : A => B) : Option[B] = this match  {
    case None => None
    case Some(a) => Some(f(a))
  }
  def getOrElse[B >: A](default: => B) : B = this match {
    case None => default
    case Some(a) => a
  }
  def filter(f : A => Boolean) : Option[A] = map{
    a : A => if(f(a)) a else a
  }

  def filter2(f : A => Boolean) : Option[A] = this match {
    case None => None
    case Some(a) => if(f(a)) Some(a) else None
  }

  def orElse[B >: A](ob: => Option[B]) : Option[B]  = map(Some(_)) getOrElse ob
}
case class Some[+A](get :A ) extends Option[A]

case object None extends Option[Nothing]

object Option {

  def mean(a : Seq[Int]) : Option[Double] = if(a.isEmpty) None else Some(a.sum /(a.length*1.0))
}
