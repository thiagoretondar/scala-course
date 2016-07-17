object session4 {

  def sum(xs: List[Int]): Int = xs match {
    case Nil => 0
    case y :: ys => y + sum(ys)
  }
  sum(List(1,1,1,1)) equals 4

  def sum2(xs: List[Int]) = (0 :: xs) reduceLeft((x, y) => x + y)
  sum2(List(1,1,1,1)) equals 4

  def product(xs: List[Int]) = (1 :: xs) reduceLeft((x, y) => x * y)
  product(List(1,1,1,4)) equals 4

  def sum3(xs: List[Int]) = (0 :: xs) reduceLeft (_ + _)
  sum3(List(1,1,1,1)) equals 4

  def product2(xs: List[Int]) = (1 :: xs) reduceLeft (_ * _)
  product2(List(1,1,1,4)) equals 4

  def sum4(xs: List[Int]) = (xs foldLeft 0) (_ + _)
  sum4(List(1,1,1,1)) equals 4

  def concat[T](xs: List[T], ys: List[T]): List[T] =
    (xs foldRight  ys) (_ :: _)

  concat(List("a", "b"), List("c", "d"))
}