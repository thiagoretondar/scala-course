object session1 {

  abstract class Int {

    def + (that: Double): Double
    def + (that: Float): Float
    def + (that: Long): Long
    def + (that: Int): Int

    def << (cnt: Int): Int

    def & (that: Long): Long
    def & (that: Int): Int

    def == (that: Double): Boolean
    def == (that: Float): Boolean
    def == (that: Long): Boolean

  }

  abstract class Nat {
    def isZero: Boolean
    def predecessor: Nat
    def sucessor: Nat = new Succ(this);
    def + (that: Nat): Nat
    def - (that: Nat): Nat
  }

  object Zero extends Nat {
    override def isZero: Boolean = true

    override def +(that: Nat): Nat = that

    override def -(that: Nat): Nat = if (that.isZero) this else throw new Error("negative number")

    override def predecessor: Nat = throw new Error("0.predecessor")
  }

  class Succ(n: Nat) extends Nat {
    override def isZero: Boolean = false

    override def +(that: Nat): Nat = new Succ(n + that)

    override def -(that: Nat): Nat = if (that.isZero) this else n - that.predecessor

    override def predecessor: Nat = n
  }

}