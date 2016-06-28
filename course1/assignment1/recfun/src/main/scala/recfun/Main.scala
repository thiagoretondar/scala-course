package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
    * Exercise 1
    */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || c == r) 1
    else if (c < 0 || c > r) 0
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {

    def parenthesesCounter(count: Int, ch: Char, ls: List[Char]): Int = {
      var newCnt = count

      if (ch == '(') newCnt = count + 1
      else if (ch == ')')  newCnt = count - 1

      if (newCnt < 0 || ls.isEmpty) newCnt
      else parenthesesCounter(newCnt, ls.head, ls.tail)
    }

    parenthesesCounter(0, chars.head, chars.tail) == 0
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, cointList: List[Int]): Int = {
    if (money == 0) 1
    else if (money < 0 || cointList.isEmpty) 0
    else countChange(money, cointList.tail) + countChange(money - cointList.head, cointList)
  }

}
