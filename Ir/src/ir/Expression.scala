package ir
import ir.types._

sealed trait Expression
sealed trait Call extends Expression
case class CallByName(called: Name) extends Call
case class CallByRef(called: Reference) extends Call
case class CallLib(called: LibFun, returning: Type) extends Call
case class Copy(to: Location, from: Value) extends Expression
case class Test(toTest: Local, ifTrue: CanCall, ifFalse: CanCall) extends Expression