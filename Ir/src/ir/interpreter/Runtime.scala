package ir.interpreter
import ir.types._
import ir._

object Runtime {
  trait RuntimeTypeImplicit[T] {
    def deserialise(bytes: Seq[Byte]): T
  }
  trait RuntimeValueImplicit[T <: Value] {
    def serialise: Seq[Byte]
  }
  implicit class RuntimeObjType(obj: ObjType) extends RuntimeTypeImplicit[ObjType] {
    def deserialise(bytes: Seq[Byte]): ObjType = ???
  }
}