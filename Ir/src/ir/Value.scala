package ir
import ir.types._

sealed trait Value

sealed trait Constant extends Value {
  def ofType: Type
}
case class Literal(value: Any, ofType: Type) extends Constant

sealed trait Location extends Value
sealed trait Register extends Location
sealed trait Memory extends Location
sealed trait CanCall
case class Stack(id: Int) extends Register with Memory
class Name(name: String, _type: => Type) extends Memory with CanCall with Constant {
  def ofType: Type = _type
}
case class Local(id: Int) extends Register
case class Reference(localReg: Int) extends Location with CanCall
case class Address(of: Memory) extends Value
case class AnonFun(exprs: Seq[Expression]) extends Constant {
  def ofType: Type = FunType(Nil, Empty)
}