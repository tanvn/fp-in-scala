package pis_chapter6

class Rational(n: Int, d: Int) {

  require(d != 0)

  private val g = gcd(n.abs, d.abs)
  val numer = n / g
  val denom = d / g

  def this(n: Int) = this(n, 1)

  def add(that: Rational): Rational =
    new Rational(
      numer * that.denom + that.numer * denom,
      denom * that.denom
    )

  override def toString = numer + "/" + denom

  def +(that: Rational): Rational =
    new Rational(
      numer * that.denom + that.numer * denom,
      denom * that.denom
    )

  def +(i: Int): Rational =
    new Rational(numer + i * denom, denom)

  def -(that: Rational): Rational =
    new Rational(
      numer * that.denom - that.numer * denom,
      denom * that.denom
    )

  def -(i: Int): Rational =
    new Rational(numer - i * denom, denom)

  def *(that: Rational): Rational =
    new Rational(numer * that.numer, denom * that.denom)

  def *(i: Int): Rational =
    new Rational(numer * i, denom)

  def /(that: Rational): Rational =
    new Rational(numer * that.denom, denom * that.numer)

  def /(i: Int): Rational =
    new Rational(numer, denom * i)

  private def gcd(a: Int, b: Int): Int =
    if (b == 0) a else gcd(b, a % b)
}

object Rational {

  implicit def intToRational(x: Int): Rational = new Rational(x)

  def main(args: Array[String]): Unit = {
    val r1 = new Rational(2, 4)
    val r2 = new Rational(2, 5)
    val r3 = new Rational(7, 15)
    println(r1 + r2)
    println(r1 * r2)
    println(r3 + r2 * r1)
    println(r3 - 1)
    println(r3 - r2)
    val r4 = 3 - r2
    println(r4)

  }
}
