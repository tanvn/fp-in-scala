package chapter6

trait RandomGenerator {
  def nextInt: (Int, RandomGenerator)
  def nonNegativeInt: (Int, RandomGenerator)
  def nextStr: (String, RandomGenerator)

}
