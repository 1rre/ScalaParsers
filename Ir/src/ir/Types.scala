package ir.types

import asm.AsmSchema
import ir.Literal

sealed abstract class Type(val size: Int) {
  def sizeSuffix: String = ""
  def typeSuffix: String = ""
  def suffix: String = s"$sizeSuffix$typeSuffix"
  //def toLiteral[T](value: T): Literal
}
sealed trait AsmType extends Type
sealed abstract class Size64 extends Type(64) {
  override def sizeSuffix: String = "d"
}
sealed abstract class Size32 extends Type(32) {
  override def sizeSuffix: String = "w"
}
sealed abstract class Size16 extends Type(16) {
  override def sizeSuffix: String = "h"
}
sealed abstract class Size8 extends Type(8) {
  override def sizeSuffix: String = "b"
}
sealed abstract trait SInt extends Type
sealed abstract trait UInt extends Type {
  override def typeSuffix: String = "u"
}
sealed abstract trait FP extends Type {
  override def typeSuffix: String = "."
}

case object Dynamic extends Type(0)
case object F64 extends Size64 with FP with AsmType
case object F32 extends Size32 with FP with AsmType {
  override def sizeSuffix: String = "s"
} 
case object S64 extends Size64 with SInt with AsmType
case object S32 extends Size32 with SInt with AsmType
case object S16 extends Size16 with SInt with AsmType
case object S8 extends Size8 with SInt with AsmType
case object U64 extends Size64 with UInt with AsmType
case object U32 extends Size32 with UInt with AsmType
case object U16 extends Size16 with UInt with AsmType
case object U8 extends Size8 with UInt with AsmType

case class ObjType(members: Seq[Type]) extends Type(members.map(_.size).sum)
case class PtrType(level: Int, ptrType: Type) extends Type(32)
case object Empty extends Type(0)
case class FunType(args: Seq[Type], returns: Type) extends Type(0)