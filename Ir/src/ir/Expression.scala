package ir
import types._
import value._

sealed trait Expression
sealed trait Call extends Expression
sealed abstract class CallResolved extends Call {
  val called: Fun
}
final class FunCallDelayed(_called: => Fun) extends CallResolved {
  lazy val called = _called
}
case class CallUnresolved(called: String) extends Call
case class FunCall(called: Fun) extends CallResolved

case class Copy(to: Location, from: Value) extends Expression
