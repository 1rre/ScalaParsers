package ir

import scala.util.parsing.combinator.RegexParsers
import types._
import value._
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
  def objType: Parser[Type] = (
    "obj[" ~> repsep(validType, ",") <~ "]" ^^ (Obj(_))
  )
  def objValue: Parser[Constant] = (
    "obj[" ~> repsep(constant, ",") <~ "]" ^^ (rs => Constant(rs, Obj(rs.map(_.typeOf))))
  )
  def validType: Parser[Type] = constType ||| objType ||| ptrType
  def constant = (((constType ~ ("[" ~> (float ||| integer))) ^^ {
    case t ~ a => Constant(a, t)
  }) <~ "]")  ||| objValue

  val funs = collection.mutable.Buffer[String]()

  def identifier = "[a-zA-Z_$0-9]+".r ^^ identity

  def call: Parser[Expression] = callLocal ||| callExt ||| callInt
  def callLocal: Parser[Call] = "call" ~> identifier ^^ (CallUnresolved(_))
  def callExt: Parser[Call] = "call" ~ "/" ~> integer ~ identifier ^^ { case a ~ b =>
    FunCall(ExternalFun(b, a.toInt))
  }
  def internalFun =
    "+" ^^^ FunCall(Add) | "-" ^^^ FunCall(Add) | "*" ^^^ FunCall(Add) | "/" ^^^ FunCall(Add)
  def callInt: Parser[FunCall] = "call" ~> internalFun

  def value: Parser[Value] = addr ||| validDest ||| constant
  def validDest = ref ||| register
  def register = local ||| stack
  def stack = "s[0-9]+".r ^^ (s => Stack(s.tail.toInt))
  def local = "r[0-9]+".r ^^ (s => Local(s.tail.toInt))
  def ref = "@[" ~> local <~ "]" ^^ (r => Reference(r.id))
  def addr = "@[" ~> stack <~ "]" ^^ (r => Address(r.id))
  def copyToMemory: Parser[Copy] = ((stack ||| ref) <~ "<=") ~ local ^^ { case a ~ b => Copy(a, b) }
  def copyToReg: Parser[Copy] =
    (local <~ "<=") ~ value ^^ { case a ~ b => Copy(a, b) }
  def copy: Parser[Copy] = copyToMemory ||| copyToReg
  def localFun: Parser[Fun] = "fun" ~> identifier ~ ("(" ~> expr.* <~ ")") ^^ { case a ~ b =>
    UnoptLocalFun(a, b)
  }
  def pubArgType: Parser[Type] = validType 
  def publishedFun: Parser[Fun] = "pub" ~> identifier ~ validType.* ~ ("(" ~> expr.* <~ ")") ^^ {
    case a ~ b ~ c => UnoptPublishedFun(a, b, c)
  }
  def fun: Parser[Fun] = localFun ||| publishedFun
  def expr: Parser[Expression] = copy | call

}
