object listfun {

  val nums = List(2, -4, 5, 7, 1)
  val fruits = List("apple", "pineapple", "orange", "banana")

  val n1 = nums filter (x => x > 0)
  val n2 = nums filterNot (x => x > 0)
  val n3 = nums partition (x => x > 0)

  (n1, n2) equals n3

  val n4 = nums takeWhile (x => x > 0)
  val n5 = nums dropWhile (x => x > 0)
  val n6 = nums span(x => x > 0)

  (n4, n5) equals n6


  val data = List("a", "a", "a", "b", "c", "c", "a")
  def pack[T](xs: List[T]): List[List[T]] = xs match {
    case Nil => Nil
    case x :: xs1 =>
      val (first, rest) = xs span (y => y == x)
      first :: pack(rest)
  }

  pack(data)

  def encode[T](xs: List[T]): List[(T, Int)] =
    pack(xs) map (ys => (ys.head, ys.length))

  encode(data)

}