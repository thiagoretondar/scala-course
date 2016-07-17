object session2 {

  def mergesort(xs: List[Int]): List[Int] = {
    val n = xs.length / 2
    if (n equals 0) xs
    else {
      def merge(xs: List[Int], ys: List[Int]): List[Int] = xs match {
        case Nil => ys
        case x :: xs1 => ys match {
          case Nil => xs
          case y :: ys1 =>
            if (x < y) x :: merge(xs1, ys)
            else y :: merge(xs, ys1)
        }
      }

      val (fst, snd) = xs splitAt n
      merge(mergesort(fst), mergesort(snd))
    }
  }

  // pair and tuples
  val pair = ("answer", 42)
  val (label, value) = pair

  def mergesort2(xs: List[Int]): List[Int] = {
    val n = xs.length / 2
    if (n == 0) xs
    else {
      def merge(xs: List[Int], ys: List[Int]): List[Int] = (xs, ys) match {
        case (Nil, ys) => ys
        case (xs, Nil) => xs
        case (x :: xs1, y :: ys1) =>
          if (x < y) x :: merge(xs1, ys)
          else y :: merge(xs, ys1)
      }

      val (fst, snd) = xs splitAt n
      merge(mergesort2(fst), mergesort2(snd))
    }
  }

  val nums = List(2, -4, 5, 7, 1)
  mergesort2(nums)
  mergesort2(nums)

  // making mergesort generic, not just for ints

  def mergesort3[T](xs: List[T])(lt: (T, T) => Boolean): List[T] = {
    val n = xs.length / 2
    if (n == 0) xs
    else {
      def merge(xs: List[T], ys: List[T]): List[T] = (xs, ys) match {
        case (Nil, ys) => ys
        case (xs, Nil) => xs
        case (x :: xs1, y :: ys1) =>
          if (lt(x, y)) x :: merge(xs1, ys)
          else y :: merge(xs, ys1)
      }

      val (fst, snd) = xs splitAt n
      merge(mergesort3(fst)(lt), mergesort3(snd)(lt))
    }
  }

  mergesort3(nums)

  // making with ordered


  def mergesort4[T](xs: List[T])(implicit ord: Ordering[T]): List[T] = {
    val n = xs.length / 2
    if (n == 0) xs
    else {
      def merge(xs: List[T], ys: List[T]): List[T] = (xs, ys) match {
        case (Nil, ys) => ys
        case (xs, Nil) => xs
        case (x :: xs1, y :: ys1) =>
          if (ord.lt(x, y)) x :: merge(xs1, ys)
          else y :: merge(xs, ys1)
      }

      val (fst, snd) = xs splitAt n
      merge(mergesort4(fst), mergesort4(snd))
    }
  }

  mergesort4(nums)

}