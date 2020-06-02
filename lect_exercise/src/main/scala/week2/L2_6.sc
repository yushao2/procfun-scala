import scala.annotation.tailrec

class Rational(x: Int, y:Int) {
  // Guard against 0 denominator
  require(y != 0, "Denominator must be zero")
  @tailrec
  private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd (b, a % b)

  // Alternative constructor for rational
  def this(x: Int) = this(x, 1)

  def numer = x
  def denom = y

  def less(that: Rational) = numer * that.denom < that.numer * denom

  // this is reference to current object
  def max(that: Rational) = if (this.less(that)) that else this

  def add(that: Rational) =
    new Rational(
      numer * that.denom + that.numer * denom,
      denom * that.denom)

  override def toString = {
    val g = gcd(numer,denom)
    numer/g + "/" + denom/g

  }

  def neg =
    new Rational(-1*numer, denom)

  def sub(that: Rational) =
    add(that.neg)
}

val x = new Rational(1,3)
val y = new Rational(5,7)
val z = new Rational(3,2)

y.add(y)

val strange = new Rational(1,0)