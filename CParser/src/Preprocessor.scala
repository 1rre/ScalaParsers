import es.tmoor.parsing._
import Preprocessor.PPKeyword
import Preprocessor.PPWhile
object Preprocessor extends Parsers[Char] {
  trait PPToken
  case object WhiteSpace extends PPToken
  case class PPNumber(s: String) extends PPToken
  case class PPSymbol(s: String) extends PPToken
  class PPKeyword(s: String) extends PPToken {
    def parser: Parser[PPToken] = s
      .map(ElemParser(_) #> (_.toString))
      .reduce(_ ~ _ #> { case (a, b) => a + b }) >> this
  }
  case object PPAuto extends PPKeyword("auto")
  case object PPDouble extends PPKeyword("double")
  case object PPInt extends PPKeyword("int")
  case object PPStruct extends PPKeyword("struct")
  case object PPBreak extends PPKeyword("break")
  case object PPElse extends PPKeyword("else")
  case object PPLong extends PPKeyword("long")
  case object PPSwitch extends PPKeyword("switch")
  case object PPCase extends PPKeyword("case")
  case object PPChar extends PPKeyword("char")
  case object PPEnum extends PPKeyword("enum")
  case object PPRegister extends PPKeyword("register")
  case object PPTypedef extends PPKeyword("typedef")
  case object PPExtern extends PPKeyword("extern")
  case object PPReturn extends PPKeyword("return")
  case object PPUnion extends PPKeyword("union")
  case object PPConst extends PPKeyword("const")
  case object PPFloat extends PPKeyword("float")
  case object PPShort extends PPKeyword("short")
  case object PPUnsigned extends PPKeyword("unsigned")
  case object PPContinue extends PPKeyword("continue")
  case object PPFor extends PPKeyword("for")
  case object PPSigned extends PPKeyword("signed")
  case object PPVoid extends PPKeyword("void")
  case object PPDefault extends PPKeyword("default")
  case object PPGoto extends PPKeyword("goto")
  case object PPSizeof extends PPKeyword("sizeof")
  case object PPVolatile extends PPKeyword("volatile")
  case object PPDo extends PPKeyword("do")
  case object PPIf extends PPKeyword("if")
  case object PPStatic extends PPKeyword("static")
  case object PPWhile extends PPKeyword("while")

  def kw: Parser[PPToken] = (
    PPAuto.parser | PPDouble.parser | PPInt.parser | PPStruct.parser | PPBreak.parser | PPElse.parser
    | PPLong.parser | PPSwitch.parser | PPCase.parser | PPChar.parser | PPEnum.parser | PPRegister.parser
    | PPTypedef.parser | PPExtern.parser | PPReturn.parser | PPUnion.parser | PPConst.parser | PPFloat.parser
    | PPShort.parser | PPUnsigned.parser | PPContinue.parser | PPFor.parser | PPSigned.parser | PPVoid.parser
    | PPDefault.parser | PPGoto.parser | PPSizeof.parser | PPVolatile.parser | PPDo.parser | PPIf.parser
    | PPStatic.parser | PPWhile.parser
  )
  case class PPIdent(s: String) extends PPToken
  case class PPCharConst(s: String)
  def lineComment =
    ElemParser('/') ~ ElemParser('/') ~ PFParser(_ != '\n').* >> WhiteSpace
  def blockComment = ElemParser('/') ~ ElemParser('*') ~ (PFParser(
    _ != '*'
  ) | (ElemParser('*') /> PFParser(_ != '/'))).* ~ ElemParser('*') ~ ElemParser(
    '/'
  ) >> WhiteSpace
  def comment = lineComment | blockComment
  def wsChar = PFParser(_.isWhitespace) >> WhiteSpace
  def ws: Parser[PPToken] =
    (wsChar | lineComment | blockComment).+ >> WhiteSpace
  def digit: Parser[Char] = PFParser(_.isDigit)
  def number: Parser[PPNumber] =  digit #> (ch => PPNumber(ch.toString))
  def token: Parser[Seq[PPToken]] = (kw | ws).*
}
