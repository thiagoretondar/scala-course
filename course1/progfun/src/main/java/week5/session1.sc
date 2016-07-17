object session1 {

  val l1 = List(1, 2, 3)
  val l2 = List(4, 5, 6)
  val l3 = l1 ++ l2

  l1 indexOf 3
  l1(1)

  l1.take(l1.length - 1)

  // My last
  def last[T](xs: List[T]): T = xs match {
    case List() => throw new Error("last of empty list")
    case List(x) => x
    case y :: ys => last(ys)
  }

  last(l1) equals l1.last

  // My init
  def init[T](xs: List[T]): List[T] = xs match {
    case List() => throw new Error("init of empty list")
    case List(x) => List(x)
    case y :: ys => y :: init(ys)
  }

  l1.init equals init(l1)

  // My concat
  def concat[T](xs: List[T], ys: List[T]): List[T] = xs match {
    case List() => ys
    case z :: zs => z :: concat(zs, ys)
  }

  l1 ::: l2 equals concat(l1, l2)
  l1 ++ l2 equals concat(l1, l2)

  // My reverse
  def reverse[T](xs: List[T]): List[T] = xs match {
    case List() => xs
    case y :: ys => reverse(ys) ++ List(y)
  }

  l1.reverse == reverse(l1)
  l2.reverse == reverse(l2)

  // My removeAt
  def removeAt[T](n: Int, xs: List[T]) = (xs take n) ::: (xs drop n + 1)

}