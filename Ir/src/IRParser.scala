import scala.util.parsing.combinator.RegexParsers


object IRParser extends RegexParsers {
  final val SizeofPtr = 32

  case class Constant(value: Any, typeOf: Type)
  override def skipWhitespace: Boolean = true
  def character = "[a-zA-Z0-9_$]".r ^^ identity
  def integer = "[0-9]+".r ^^ (_.toLong)
  def float = """[0-9]+\.[0-9]+""".r ^^ (_.toDouble)
  abstract class Type(val isFloat: Boolean, val isSigned: Boolean, val size: Int)
  sealed class Unsigned(_size: Int) extends Type(false, false, _size)
  sealed class Signed(_size: Int) extends Type(false, true, _size)
  sealed class Floating(_size: Int) extends Type(false, true, _size)
  case class Ptr(level: Int, ptrType: Type) extends Type(false, false, SizeofPtr)
  case class Obj(members: Seq[Type]) extends Type(false, false, members.map(_.size).sum)
  case object U8 extends Unsigned(8)
  case object S8 extends Signed(8)
  case object U16 extends Unsigned(16)
  case object S16 extends Signed(16)
  case object U32 extends Unsigned(32)
  case object S32 extends Signed(32)
  case object F32 extends Floating(32)
  case object U64 extends Unsigned(64)
  case object S64 extends Signed(64)
  case object F64 extends Floating(64)
  def constType: Parser[Type] = (
    ("u8"^^^U8) | ("u16"^^^U16) | ("u32"^^^U32) | ("u64"^^^U64)
    | ("s8"^^^S8) | ("s16"^^^S16) | ("s32"^^^S32) | ("s64"^^^S64)
    | ("f32"^^^F32) | ("f64"^^^F64)
  )
  def objType: Parser[Type] = (
    "obj[" ~> repsep(validType, ",") <~ "]" ^^ (Obj(_))
  )
  def objValue: Parser[Constant] = (
    "obj[" ~> repsep(constant, ",") <~ "]" ^^ (rs => Constant(rs, Obj(rs.map(_.typeOf))))
  )
  def ptrType: Parser[Type] = (
    """>[0-9]*\(""".r ~ validType <~ ")" ^^ {
      case a ~ b => {
        val ptrDepth = a.tail.init.toIntOption.getOrElse(1)
        Ptr(ptrDepth, b)
      }
    }
  )
  def validType: Parser[Type] = constType | objType | ptrType
  def typeSpecifier = """\#""".r ~> validType
  def constant = (float | integer) ~ typeSpecifier ^^ {
    case a ~ t => Constant(a, t)
  } | objValue

  trait Expr

  case class Fun(exprs: Seq[Expr])
  val funs = collection.mutable.Map[String, Fun]()

  trait FunCall extends Expr
  case class FunCallLocal(fun: Fun) extends FunCall {
    def this(s: String) = this(funs(s))
  }
  case class FunCallExternal(fun: String, nArgs: Long) extends FunCall
  trait InternalFun extends FunCall
  case object Add extends InternalFun
  case object Sub extends InternalFun
  case object Mul extends InternalFun
  case object Div extends InternalFun

  def identifier = "[a-zA-Z_$0-9]+".r ^^ identity

  def call: Parser[FunCall] = callLocal ||| callExt ||| callInt
  def callLocal: Parser[FunCallLocal] = "call" ~> identifier ^^ (FunCallLocal(_))
  def callExt: Parser[FunCallExternal] = "call" ~ "/" ~> integer ~ identifier ^^ {
    case a ~ b => FunCallExternal(b,a)
  }
  def internalFun = "+" ^^^ Add | "-" ^^^ Sub | "*" ^^^ Mul | "/" ^^^ Div
  def callInt: Parser[InternalFun] = "call" ~> internalFun

  
}