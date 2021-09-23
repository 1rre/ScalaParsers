package ir

import ir.types.Type

sealed trait GlobalStatement

sealed trait Fun extends GlobalStatement {
  val name: String
  val isPublished = false
}
sealed trait HasArgs extends Fun {
  val nArgs: Int
}
sealed trait DeclaredFun extends Fun {
  val exprs: Seq[Expression]
}
case class OptFun(name: String, exprs: Seq[Expression], nArgs: Int, maxReg: Int, maxStack: Int)
    extends DeclaredFun
    with HasArgs
sealed abstract trait UnoptFun extends DeclaredFun {
  val name: String
  val exprs: Seq[Expression]
}
case class UnoptPublishedFun(name: String, args: Seq[Type], exprs: Seq[Expression]) extends UnoptFun
case class UnoptLocalFun(name: String, exprs: Seq[Expression]) extends UnoptFun
case class InternalFun(name: String, nArgs: Int) extends Fun with HasArgs
case class ExternalFun(name: String, nArgs: Int) extends Fun with HasArgs
object Add extends InternalFun("+", 2)
object Sub extends InternalFun("-", 2)
object Mul extends InternalFun("*", 2)
object Div extends InternalFun("/", 2)
