import scala.math.Numeric.Implicits.infixNumericOps

class LinEq[T](val a: List[T], val x: List[T], val b: T) {
  def solve(implicit ops: LinEqOps[T]): Boolean = {
    var sum = ops.zero
    for ((a_el, x_el) <- a.zip(x)) {
      sum = ops.+(sum, ops.*(a_el, x_el))
    }
    ops.<(sum, b)
  }
}

abstract class LinEqOps[T] {
  def *(a: T, b: T): T
  def +(a: T, b: T): T
  def zero: T
  def <(a: T, b: T): Boolean
}

object LinEqOps {
  implicit def numeric[T](implicit num: Numeric[T]): LinEqOps[T] = new LinEqOps[T] {
    override def *(a: T, b: T): T = a * b
    override def +(a: T, b: T): T = a + b
    override def zero: T = num.zero
    override def <(a: T, b: T): Boolean = num.lt(a, b)
  }

  implicit object bool extends LinEqOps[Boolean] {
    override def *(a: Boolean, b: Boolean): Boolean = a && b
    override def +(a: Boolean, b: Boolean): Boolean = a || b
    override def zero: Boolean = false
    override def <(a: Boolean, b: Boolean): Boolean = !a && b
  }
}

object Main {
  def main(args: Array[String]): Unit = {
    val eq1 = new LinEq(List(1, 2, 3), List(3, 4, 6), 30)
    val eq2 = new LinEq(List(false, true, false), List(false, false, true), true)
    val eq3 = new LinEq(List(false, true, false), List(false, true, true), true)
    val eq4 = new LinEq(List(1, 2, 3), List(3, 4, 6), 26)

    println(eq1.solve)
    println(eq2.solve)
    println(eq3.solve)
    println(eq4.solve)
  }
}