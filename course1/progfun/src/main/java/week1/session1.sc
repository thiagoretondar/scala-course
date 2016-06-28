object session1 {

  def abs(x: Double) = if (x < 0) -x else x

  /**
    * Defining the sqrt function using the Newton's algorithm
    *
    * @param x number
    * @return
    */
  def sqrt(x: Double) = {

    def sqrtIter(guess: Double): Double = { // put the return type because is recursive
      if (isGoodEnough(guess)) guess
      else sqrtIter(improve(guess))
    }

    def isGoodEnough(guess: Double) = {
      abs(guess * guess - x) / x < 0.001
    }

    def improve(guess: Double) = {
      (guess + x / guess) / 2
    }

    sqrtIter(1.0)
  }

  sqrt(2)
  sqrt(4)
  sqrt(9)
  sqrt(1e-6)
  sqrt(1e60)

}