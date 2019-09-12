package chapter8

case class SGen[A](forSize: Int => Gen[A])
