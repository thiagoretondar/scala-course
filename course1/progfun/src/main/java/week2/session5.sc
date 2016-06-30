object session5 {

  class Rational(x: Int, y: Int) {

    require(y != 0, "denominator must be nonzero")

    def this(x: Int) = this(x, 1)

    private def gcd(a: Int, b: Int): Int =
      if (b == 0) a else gcd(b, a % b)

    val numer = x

    val denom = y

    def add(that: Rational) =
      new Rational(
        numer * that.denom + that.numer * denom,
        denom * that.denom)

    def neg: Rational = new Rational(-numer, denom)

    def sub(that: Rational) = add(that.neg)

    def less(that: Rational) =
      this.numer * that.denom < that.numer * this.denom

    def max(that: Rational) =
      if (this.less(that)) that else this



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
  x1.add(y)




  val x = new Rational(1, 3)
  val y = new Rational(5, 7)
  val z = new Rational(3, 2)
  x.sub(y).sub(z)
  y.add(y)

  x.less(y)

  x.max(y)

  val strange = new Rational(1, 0)
  strange.add(strange)

  new Rational(2)

}

