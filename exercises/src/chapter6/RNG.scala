package chapter6

trait RNG {
  def nextInt: (Int, RNG)
  def nonNegativeInt: (Int, RNG)

}
