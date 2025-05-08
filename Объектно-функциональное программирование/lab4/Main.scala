abstract class Expr
case class SumExpr(a: Expr, b: Expr) extends Expr
case class MulExpr(a: Expr, b: Expr) extends Expr
case class VarExpr(name: String) extends Expr
case class LetExpr(name: String, exp: Expr, body: Expr) extends Expr


object Main {
  var count = 0
  def letsOptimize(e: Expr): Expr = e match {
    case SumExpr(a, b) =>
      if (a.equals(b)) {
        count += 1
        LetExpr("v" + count, letsOptimize(a),
          SumExpr(VarExpr("v" + count), VarExpr("v" + count)))
      } else {
        SumExpr(letsOptimize(a), letsOptimize(b))
      }
    case MulExpr(a, b) =>
      if (a.equals(b)) {
        count += 1
        LetExpr("v" + count, letsOptimize(a),
          MulExpr(VarExpr("v" + count), VarExpr("v" + count)))
      } else {
        MulExpr(letsOptimize(a), letsOptimize(b))
      }
    case LetExpr(name, expr, body) =>
      val optLeft  = letsOptimize(expr)
      val optRight = letsOptimize(body)
      countName(name, optRight) match {
        case 0 =>
          optRight
        case 1 =>
          substitute(optRight, name, optLeft)
        case _ =>
          LetExpr(name, optLeft, optRight)
      }
    case VarExpr(name) =>
      VarExpr(name)
  }

  def substitute(expr: Expr, name: String, sub: Expr): Expr = expr match {
    case VarExpr(n) =>
      if (n == name) sub else VarExpr(n)
    case SumExpr(a, b) =>
      SumExpr(substitute(a, name, sub), substitute(b, name, sub))
    case MulExpr(a, b) =>
      MulExpr(substitute(a, name, sub), substitute(b, name, sub))
    case LetExpr(n, l, r) =>
      if (n == name) LetExpr(n, substitute(l, name, sub), r)
      else LetExpr(n, substitute(l, name, sub), substitute(r, name, sub))
  }

  def countName(name: String, expr: Expr): Int = expr match {
    case VarExpr(n) =>
      if (n == name) 1 else 0
    case SumExpr(a, b) =>
      countName(name, a) + countName(name, b)
    case MulExpr(a, b) =>
      countName(name, a) + countName(name, b)
    case LetExpr(n, l, r) =>
      // если let переопределяет имя
      if (n == name) countName(name, l)
      else countName(name, l) + countName(name, r)
  }

  def main(args: Array[String]): Unit = {
    println(letsOptimize(MulExpr(SumExpr(VarExpr("x"), VarExpr("y")),
      SumExpr(VarExpr("x"), VarExpr("y")))))
    println(letsOptimize(LetExpr("x", SumExpr(VarExpr("y"), VarExpr("z")),
      MulExpr(VarExpr("a"), VarExpr("x")))))
  }
}
