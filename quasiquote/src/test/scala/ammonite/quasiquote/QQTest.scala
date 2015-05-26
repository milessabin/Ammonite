package ammonite.quasiquote

import scala.reflect.api.Universe

object QQTest{
  def main(args: Array[String]): Unit = {
    import scala.tools.reflect.ToolBox
    val cm = scala.reflect.runtime.currentMirror
    val tb = cm.mkToolBox()
    implicit val u = cm.universe

    assert(tb.eval(QQ(1 + "lol").cTree) == "1lol")
    assert(
      u.showRaw(QQ(1 + " " + 1)) ==
      """QTree(Apply(Select(Apply(Select(Literal(Constant(1)), TermName("$plus")), List(Literal(Constant(" ")))), TermName("$plus")), List(Literal(Constant(1)))))"""
    )

    val first = QQ(1 + "omg")
    val second = QQ(2 + "wtf")
    val mixed = QQ(AST(first) + AST(second))

    assert(tb.eval(mixed.cTree) == "1omg2wtf")

    val lifter = QLifters[u.type](cm.universe)(QQ.universeExpr(u))
    import lifter._
    val x = 1
    val y = 2
    val unquoted = QQ(UQ(x + y))

    assert(u.showRaw(unquoted) == "QTree(Literal(Constant(3)))")
  }
}

