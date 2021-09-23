package mips

trait Register
sealed class IntRegister(id: Int) extends Register {
  override def toString(): String = s"$$$id"
}
sealed class FloatRegister(id: Int) extends Register {
  override def toString(): String = s"$$f$id"
}
case object $0 extends IntRegister(0)
case object $AT extends IntRegister(1)
case object $V0 extends IntRegister(2)
case object $V1 extends IntRegister(3)
case object $A0 extends IntRegister(4)
case object $A1 extends IntRegister(5)
case object $A2 extends IntRegister(6)
case object $A3 extends IntRegister(7)
case object $T0 extends IntRegister(8)
case object $T1 extends IntRegister(9)
case object $T2 extends IntRegister(10)
case object $T3 extends IntRegister(11)
case object $T4 extends IntRegister(12)
case object $T5 extends IntRegister(13)
case object $T6 extends IntRegister(14)
case object $T7 extends IntRegister(15)
case object $S0 extends IntRegister(16)
case object $S1 extends IntRegister(17)
case object $S2 extends IntRegister(18)
case object $S3 extends IntRegister(19)
case object $S4 extends IntRegister(20)
case object $S5 extends IntRegister(21)
case object $S6 extends IntRegister(22)
case object $S7 extends IntRegister(23)
case object $T8 extends IntRegister(24)
case object $T9 extends IntRegister(25)
case object $KT0 extends IntRegister(26)
case object $KT1 extends IntRegister(27)
case object $GP extends IntRegister(28)
case object $SP extends IntRegister(29)
case object $S8 extends IntRegister(30)
case object $RA extends IntRegister(31)