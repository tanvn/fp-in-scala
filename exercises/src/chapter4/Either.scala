package chapter4

sealed trait Either[+E, +A] {

  def map[B]( f: A => B) : Either[E, B] = this match {
    case Right(v) => Right(f(v))
    case Left(e) => Left(e)

  }


  def flatMap[EE >: E, B](f : A => Either[EE, B]) : Either[EE, B]= this match {
    case Left(e) => Left(e)
    case Right(v) => f(v)
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]) : Either[EE, B]= this match {
    case Left(_) => b
    case Right(v) => Right(v)
  }

  def map2[EE >: E, B, C](b: => Either[EE, B])( f: (A,B) => C) : Either[EE, C] = flatMap{
    a => b.map(bb => f(a,bb))
  }



}
case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]


object Either {


  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = es match {
    case Right(v) :: tail =>
      sequence(tail).map(t => v :: t)
    case Left(e) :: _ => Left(e)
    case Nil => Right(List.empty[A])
  }

  def traverse[E, A, B](as: List[A])(
    f: A => Either[E, B]): Either[E, List[B]] =  as match {
    case h::tail => traverse(tail)(f).flatMap(t => f(h).map(_:: t))
    case Nil => Right(List.empty[B])
  }

  def mean(xs: IndexedSeq[Double]): Either[String, Double] =
    if (xs.isEmpty)
      Left("mean of empty list!")
    else
      Right(xs.sum / xs.length)


  def saveDiv(x : Double, y : Double) : Either[Exception, Double] = {
    try Right(x/y)
    catch{
      case e : Exception => Left(e)
    }
  }

  def main(args: Array[String]): Unit = {
    println(saveDiv(10, 0))

    val testData1 = List(Right(1), Right(2), Right(3), Right(4))
    val testData2 = List(Right(1), Right(2), Right(3),Left("Not a number"), Right(5))
    val testData3 = List(1, 2, 3,4,5)


    println(sequence(testData1))
    println(sequence(testData2))
    println(traverse(testData1)( v => v.map( a => a + 10)))
    println(traverse(testData2)( v => v.map( a => a + 10)))
    println(traverse(testData3)( v => if(v %2 ==0) Right(v/2) else Left("Not an even number")))

  }
}
