package ir.value

sealed trait Location extends Value
sealed trait Register extends Location
case class Stack(id: Int) extends Register
case class Local(id: Int) extends Register
case class Reference(localReg: Int) extends Location
case class Address(stackReg: Int) extends Location