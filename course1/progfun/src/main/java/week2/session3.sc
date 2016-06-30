object session3 {

  // function that returns another function
  def sum(f: Int => Int): (Int, Int) => Int = {
    def sumF(a: Int, b: Int): Int = {
      if (a > b) 0
      else f(a) + sumF(a + 1, b)
    }
    sumF
  }

  def sumInts = sum(x => x)
  def sumCubes = sum(x => x * x * x)
  def sumFactorial = sum(fact)

  def fact(x: Int): Int = {
    if (x == 0) 1
    else x * fact(x - 1)
  }

  // or we could write (syntax sugar)

  def sum2(f: Int => Int)(a: Int, b: Int): Int = {
    if (a > b) 0
    else f(a) + sum2(f)(a + 1, b)
  }

  def cube(x: Int) = x * x * x
  def ints(x: Int) = x

  sum (cube) (1, 2)
  sum (ints) (1, 4)

  // Execise

  def product(f : Int => Int)(a: Int, b: Int): Int = {
    if (a > b) 1
    else f(a) * product(f)(a + 1, b)
  }

  product (x => x * x) (3, 4)

  def fact2(n : Int): Int = {
    product (x => x) (1, n)
  }

  fact2(5)

  def mapReduce(f: Int => Int, combine: (Int, Int) => Int, zero: Int)(a: Int, b: Int): Int = {
    if (a > b) zero
    else combine(f(a), mapReduce(f, combine, zero)(a + 1, b))
  }

  def product2(f: Int => Int)(a: Int, b: Int): Int = mapReduce(f, (x, y) => x * y, 1)(a, b)

  product2(x => x * x)(3, 4)

}