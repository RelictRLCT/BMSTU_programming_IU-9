class FibInt(num: BigInt) {
  val fiblist: List[BigInt] = make_fib_list(num).reverse
  val fibnum: List[BigInt] = toFib(num, fiblist)

  def toFib(n: BigInt, fibs: List[BigInt]): List[BigInt] = {
    def loop(num: BigInt, fibs: List[BigInt], acc: List[BigInt]): List[BigInt] = fibs match {
      case Nil => acc.reverse
      case f :: tail =>
        if (num >= f) loop(num - f, tail, 1 :: acc)
        else loop(num, tail, 0 :: acc)
    }
    loop(n, fibs, List())
  }

  def +(second: FibInt): FibInt = {
    val res = this.toInteger() + second.toInteger()
    println(res)
    new FibInt(res)
  }

  def %(second: FibInt): FibInt = {
    val maxLen = math.max(fibnum.length, second.fibnum.length)

    def padWithZeros(lst: List[BigInt], targetLen: BigInt): List[BigInt] = {
      if (lst.length >= targetLen) lst
      else padWithZeros(0 :: lst, targetLen)
    }

    val fibA = padWithZeros(fibnum, maxLen)
    val fibB = padWithZeros(second.fibnum, maxLen)

    val newSum = (0 until maxLen).foldLeft(BigInt(0)) { (acc, i) =>
      if (fibA(i) == 1 && fibB(i) == 1) acc + fiblist(i)
      else acc
    }

    println(newSum)
    new FibInt(newSum)
  }

  def toInteger(): BigInt = {
    var sum = BigInt(0)
    for (i <- 0 to fiblist.length - 1) {
      sum += fiblist(i) * fibnum(i)
    }
    sum
  }

  def make_fib_list(num: BigInt): List[BigInt] = {
    def loop(a: BigInt, b: BigInt, acc: List[BigInt]): List[BigInt] = {
      val next = a + b
      if (next > num) acc.reverse else loop(b, next, next :: acc)
    }
    loop(1, 1, List(1, 1))
  }

}

object Main {
  var a = new FibInt(24)
  var b = new FibInt(11)
  def main(args: Array[String]): Unit = {
    var c = a + b
    var d = a % b
    println(c.toInteger(), d.toInteger())
  }
}
