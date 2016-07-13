object session2 {

//  trait Expr {
//    def isNumber: Boolean
//    def isSum: Boolean
//    def numValue: Int
//    def leftOp: Expr
//    def rightOp: Expr
//  }
//
//  class Number(n: Int) extends Expr {
//    override def isNumber: Boolean = true
//
//    override def isSum: Boolean = false
//
//    override def numValue: Int = n
//
//    override def leftOp: Expr = throw new Error("Number.rightOp")
//
//    override def rightOp: Expr = throw new Error("Number.leftOp")
//  }
//
//  class Sum(e1: Expr, e2: Expr) extends Expr {
//    override def isNumber: Boolean = false
//
//    override def isSum: Boolean = true
//
//    override def numValue: Int = throw new Error("Sum.numValue")
//
//    override def leftOp: Expr = e1
//
//    override def rightOp: Expr = e2
//  }
//
//  def eval(e: Expr): Int = {
//    if (e.isNumber) e.numValue
//    else if (e.isSum) eval(e.leftOp) + eval(e.rightOp)
//    else throw new Error("Unknow expression " + e)
//  }

  // NOW WE JUST ADD THE EVAL METHOD TO THE Expr
  // BUT NOW IF WE WANT TO ADD SOME METHOD TO
  // A SPECIFIC CLASS, ALL THE CLASSES SHOULD HAVE
  // THIS PARTICULAR METHOD

//  trait Expr {
//    def eval: Int
//  }
//
//  class Number(n: Int) extends Expr {
//    override def eval: Int = n
//  }
//
//  class Sum(e1: Expr, e2: Expr) extends Expr {
//    override def eval: Int = e1.eval + e2.eval
//  }

  // NOW USING PATTERN MATCHING

  trait Expr
  case class Number(n: Int) extends Expr
  case class Sum(e1: Expr, e2: Expr) extends Expr

  object Number {
    def apply(n: Int): Number = new Number(n)
  }
  
  object Sum {
    def apply(e1: Expr, e2: Expr): Sum = new Sum(e1, e2)
  }

  def eval(e: Expr): Int = e match {
    case Number(n) => n
    case Sum(e1, e2) => eval(e1) + eval(e2)
  }


}