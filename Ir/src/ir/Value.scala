package ir
import ir.types._

sealed trait Value

sealed trait Constant extends Value {
  def ofType: Type
}
case class Literal(value: Any, ofType: Type) extends Constant
/*
case class FLong(value: Double) extends Literal(value, F64)
case class FWord(value: Float) extends Literal(value, F32)
case class ULong(value: Long) extends Literal(value, U64)
case class UWord(value: Int) extends Literal(value, U32)
case class UShort(value: Short) extends Literal(value, U16)
case class UChar(value: Byte) extends Literal(value, U8)
case class SLong(value: Long) extends Literal(value, S64)
case class SWord(value: Int) extends Literal(value, S32)
case class SShort(value: Short) extends Literal(value, S16)
case class SChar(value: Byte) extends Literal(value, S8)
case class Obj(value: Seq[Constant], types: ObjType) extends Literal(value, types)
case class Ptr[T](value: Long, pType: PtrType) extends Literal(value, pType)
*/
sealed trait Location extends Value
sealed trait Register extends Location
sealed trait Memory extends Location
sealed trait CanCall
case class Stack(id: Int) extends Register with Memory
class Name(val name: String, _type: => Type) extends Memory with CanCall with Constant {
  def ofType: Type = _type
}
case class Local(id: Int) extends Register
case class Reference(localReg: Int) extends Location with CanCall
case class Address(of: Memory) extends Value
trait Fun extends Constant with CanCall {
  def ofType: Type = FunType(Nil, Empty)
  val exprs: Seq[Expression]
}
case class InlinedFun(exprs: Seq[Expression]) extends Fun
case class IndependentFun(exprs: Seq[Expression], name: String = "$anon") extends Fun
case class DependentFun(exprs: Seq[Expression]) extends Fun