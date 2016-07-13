object session3 {

  val fruit1 = List("apples", "oranges", "pears")
  val fruit2 = "apples" :: ("oranges" :: ("pears" :: Nil))

  def isort(xs: List[Int]): List[Int] = xs match {
    case List() => List()
    case y :: ys => insert(y, isort(ys))
  }

  def insert(x: Int, xs: List[Int]): List[Int] = xs match {
    case List() => List()
    case y :: ys => if (x <= y) x :: xs else y :: insert(y, ys)
  }

}