package mips
import ir.types._

sealed trait Expression
sealed trait Instruction extends Expression
sealed class RWInstruction(name: String, r1: Register, r2: Register, offset: Int)
    extends Instruction {
  override def toString(): String = s"$name $r1 $offset($r2)"
}
final class La(rd: IntRegister, rs: IntRegister, offset: Int)
    extends RWInstruction("la", rd, rs, offset) {
  def this(rd: IntRegister, rs: IntRegister) = this(rd, rs, 0)
  def this(rd: IntRegister, offset: Int) = this(rd, $0, 0)
}
final class Load(rd: Register, rs: IntRegister, offset: Int, typ: AsmType)
    extends RWInstruction(s"l${typ.suffix}", rd, rs, offset) {
  def this(rd: IntRegister, rs: IntRegister, typ: AsmType) = this(rd, rs, 0, typ)
  def this(rd: IntRegister, offset: Int, typ: AsmType) = this(rd, $0, 0, typ)
}
final class Store(rs: Register, rd: IntRegister, offset: Int, typ: AsmType)
    extends RWInstruction(s"s${typ.suffix}", rd, rs, offset) {
  def this(rs: Register, rd: IntRegister, typ: AsmType) = this(rs, rd, 0, typ)
  def this(rs: Register, offset: Int, typ: AsmType) = this(rs, $0, 0, typ)
}
final class Li(rd: Register, value: AnyVal, typ: AsmType) extends Instruction {
  override def toString(): String =
    if (typ.isInstanceOf[FP]) s"li${typ.suffix} $rd $value"
    else s"li $rd $value"
}
sealed class RegOperation(name: String, rd: Register, r1: Register, r2: Register, typ: AsmType)
    extends Instruction {
  override def toString(): String =
    if (typ.isInstanceOf[FP]) s"$name${typ.suffix} $rd $r1 $r2"
    else s"$name${typ.typeSuffix} $rd $r1 $r2"
}
final class Add(rd: Register, r1: Register, r2: Register, typ: AsmType)
    extends RegOperation("add", rd, r1, r2, typ)

sealed class ImOperation(name: String, rd: Register, r1: Register, im: Int, typ: AsmType)
    extends Instruction {
  override def toString(): String = s"$name${typ.typeSuffix} $rd $r1 $im"
}
final class Addi(rd: IntRegister, r1: IntRegister, im: Int, typ: AsmType)
    extends ImOperation("addi", rd, r1, im, typ)
