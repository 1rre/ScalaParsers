package ir

import ir.types._

sealed trait GlobalStatement {
  val name: String
  val ofType: Type
}

trait Callable
sealed trait DeclaredFunction extends Callable {
  val args: Seq[Type]
  val returns: Type
}
case class Decl(name: String, ofType: Type, initialValue: Option[Constant]) extends GlobalStatement
case class Ext(name: String, ofType: Type) extends GlobalStatement

sealed abstract class LibFun(name: String, nArgs: Int) extends Callable
case object Add extends LibFun("+", 2)
case object Sub extends LibFun("-", 2)
case object Mul extends LibFun("*", 2)
case object Div extends LibFun("/", 2)
