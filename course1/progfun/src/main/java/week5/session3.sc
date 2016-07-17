object session3 {

  // High order functions

  def squareList(xs: List[Int]): List[Int] = xs match {
    case Nil => xs
    case y :: ys => y * y :: squareList(ys)
  }

  val list1to5 = List(1, 2, 3, 4, 5)
  val sl = squareList(list1to5)

  def squareList2(xs: List[Int]): List[Int] =
    xs map (x => x * x)

  val sl2 = squareList2(list1to5)

  sl equals sl2


  def posElems(xs: List[Int]): List[Int] = xs match {
    case Nil => xs
    case y :: ys => if (y > 0) y :: posElems(ys) else posElems(ys)
  }

  val listWithNeg = List(-1, 2, -3, 4)
  val p1 = posElems(listWithNeg)

  def posElems2(xs: List[Int]): List[Int] = xs filter (x => x > 0)
  val p2 = posElems2(listWithNeg)

  p1 equals p2

}