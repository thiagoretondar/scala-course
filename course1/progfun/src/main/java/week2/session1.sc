object session1 {

  // Anonymous functions
  def sum(f: Int => Int, a: Int, b: Int): Int = {
    // f is function which takes an argument Int
    // and return a result of type Int
    def loop(a: Int, acc: Int): Int = {
      if (a > b) acc
      else loop(a + 1, f(a) + acc)
    }
    loop(a, 0)
  }

  // we could define this function also with this
  // params and body, but it wouldn't be anonymous
  sum(x => x * x, 3, 5)

}