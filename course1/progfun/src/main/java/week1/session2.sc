import scala.annotation.tailrec

object session2 {

  /**
    * Computes the greatest common divisor
    *
    * @param a number 1
    * @param b number 2
    * @return the greatest commom divisor
    */
  def gcd(a: Int, b: Int): Int = {
    if (b == 0) a
    else gcd(b, a % b)
  }

  gcd(14, 21)

  def factorialNotTail(n: Int): Int = {
    if (n == 0) 1
    else n * factorialNotTail(n - 1)
  }

  factorialNotTail(5)

  def factorialTail(n: Int): Int = {

    @tailrec
    def loop(acc: Int, n: Int) : Int =
      if (n == 0) acc
      else loop(acc * n, n - 1)

    loop(1, n)

  }

  factorialTail(5)

}