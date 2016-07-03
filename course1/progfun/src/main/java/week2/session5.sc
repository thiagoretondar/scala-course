object session5 {

  class Rational(x: Int, y: Int) {

    require(y != 0, "denominator must be nonzero")

    def this(x: Int) = this(x, 1)

    private def gcd(a: Int, b: Int): Int =
      if (b == 0) a else gcd(b, a % b)

    val numer = x

    val denom = y

    def + (that: Rational) =
      new Rational(
        numer * that.denom + that.numer * denom,
        denom * that.denom)

    def unary_- : Rational = new Rational(-numer, denom)

    def - (that: Rational) = this + -that

    def < (that: Rational) =
      this.numer * that.denom < that.numer * this.denom

    def max(that: Rational) =
      if (this < that) that else this

    override def toString = {
      val g = gcd(numer, denom)
      numer / g + "/" + denom / g
    }
  }

  val x1 = new Rational(1, 2)
  x1.numer
  x1.denom

  def addRational(r: Rational, s: Rational): Rational =
    new Rational(
      r.numer * s.denom + s.numer * r.denom,
      r.denom * s.denom
    )

  def makeString(r: Rational) =
    r.numer + "/" + r.denom

  makeString(addRational(new Rational(1, 2), new Rational(2, 3)))

  val y1 = new Rational(2, 3)
  x1 + y




  val x = new Rational(1, 3)
  val y = new Rational(5, 7)
  val z = new Rational(3, 2)
  x - y - z
  y + y

  x < y

  x.max(y)

  val strange = new Rational(1, 0)
  strange + strange

  new Rational(2)

  //  using without ()
  //  val t1 = new Rational(4,2)
  //  val t2 = new Rational(6,3)
  //  val t3 = t1 add t2


}

