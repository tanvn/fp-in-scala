package chapter11

trait Functor[Foo[_]] {
  def map[A, B](fa: Foo[A])(f: A => B): Foo[B]

  def distribute[A, B](fab: Foo[(A, B)]): (Foo[A], Foo[B]) =
    (map(fab)(_._1), map(fab)(_._2))

  def codistribute[A, B](e: Either[Foo[A], Foo[B]]): Foo[Either[A, B]] =
    e match {
      case Left(fa: Foo[A])  => map(fa)(Left(_))
      case Right(fb: Foo[B]) => map(fb)(Right(_))
    }
}

object Functor {

  val listFunctor = new Functor[List] {
    def map[A, B](as: List[A])(f: A => B): List[B] = as.map(f)
  }

}
