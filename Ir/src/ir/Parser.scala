package ir

import scala.util.parsing.combinator.RegexParsers
import ir.types._
import asm.Mips

object Parser extends RegexParsers {

  override def skipWhitespace: Boolean = true
  def character = "[a-zA-Z0-9_$]".r ^^ identity
  def integer = "[0-9]+".r ^^ (_.toLong)
  def float = """[0-9]+\.[0-9]+""".r ^^ (_.toDouble)

  def ptrType: Parser[Type] = "@" ~> (("(" ~> ("[0-9]+".r ^^ (_.toInt)) <~ ")").? ^^ (_.getOrElse(1))) ~ ("[" ~> validType <~ "]") ^^ {
    case i~t => Ptr(i, t)
  }
  def constType: Parser[Type] = (
    ("u8" ^^^ U8) | ("u16" ^^^ U16) | ("u32" ^^^ U32) | ("u64" ^^^ U64)
      | ("i8" ^^^ S8) | ("i16" ^^^ S16) | ("i32" ^^^ S32) | ("i64" ^^^ S64)
      | ("f32" ^^^ F32) | ("f64" ^^^ F64)
  )
  def objType: Parser[Type] =
    "obj[" ~> repsep(validType, ",") <~ "]" ^^ (Obj(_))
  def objValue: Parser[Literal] =
    "obj[" ~> repsep(constant, ",") <~ "]" ^^ (rs => Literal(rs, Obj(rs.map(_.ofType))))
  def funType: Parser[FunType] = (
    (("fun[" ~> validType <~ "]") ||| ("fun" ^^^ Empty)) ~ validType.* ^^ {
      case a ~ b => FunType(b, a)
    } 
  )
  def validType: Parser[Type] = constType ||| objType ||| ptrType ||| funType
  def literal: Parser[Literal] = (((constType ~ ("[" ~> (float ||| integer))) ^^ {
    case t ~ a => Literal(a, t)
  }) <~ "]")  ||| objValue
  def anonFun: Parser[AnonFun] = ("(" ~> expr.* <~ ")") ^^ (AnonFun(_))
  def constant = literal ||| ("@[" ~> name <~ "]") ||| anonFun

  val names = collection.mutable.Map[String, Type]()

  def identifier = "[a-zA-Z_][_a-zA-Z0-9]*".r ^^ identity

  def call: Parser[Expression] = callName ||| callInternal ||| callRef
  def callName: Parser[Call] = name ^^ (CallByName(_))
  def callRef: Parser[Call] = ref ^^ (CallByRef(_))
  def internalFun =
    "+" ^^^ CallLib(Add) | "-" ^^^ CallLib(Add) | "*" ^^^ CallLib(Add) | "/" ^^^ CallLib(Add)
  def callInternal: Parser[Call] = internalFun

  def value: Parser[Value] = addr ||| validDest ||| literal
  def validDest = ref ||| register
  def register = local ||| stack
  def stack = "s[0-9]+".r ^^ (s => Stack(s.tail.toInt))
  def local = "r[0-9]+".r ^^ (s => Local(s.tail.toInt))
  def name = identifier ^^ (id => new Name(id, names(id)))
  def ref = "@[" ~> local <~ "]" ^^ (r => Reference(r.id))
  def addr = ("@[" ~> (stack ||| name) <~ "]" ^^ (r => Address(r)))
  def copyToMemory: Parser[Copy] = ((stack ||| ref) <~ "<=") ~ local ^^ { case a ~ b => Copy(a, b) }
  def copyToReg: Parser[Copy] =
    (local <~ "<=") ~ value ^^ { case a ~ b => Copy(a, b) }
  def copy: Parser[Copy] = copyToMemory ||| copyToReg
  def decl = validType ~ identifier ~ constant.? ^^ {
    case a ~ b ~ c => Decl(b,a,c)
  }
  def ext = "ext" ~> decl ^^ (decl => Ext(decl.name, decl.ofType))
  def global = phrase((ext ||| decl).*)
  def expr: Parser[Expression] = copy | call

}
