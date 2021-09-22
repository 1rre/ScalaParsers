import es.tmoor.parsing._

object Preprocessor extends Parsers[Char] {
  trait PPToken
  case object WhiteSpace extends PPToken
  case class PPNumber(s: String) extends PPToken
  case class PPCharConst(s: String) extends PPToken
  case class PPString(s: String) extends PPToken
  class PPKeyword(s: String) extends PPToken {
    val parser: Parser[PPToken] =
      s.map(_ #> (_.toString)).reduce[Parser[_]](_ ~ _) >> this
  }
  class PPSymbol(s: String) extends PPToken {
    val parser: Parser[PPToken] =
      s.map(_ #> (_.toString)).reduce[Parser[_]](_ ~ _) >> this
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

  case object `<<=` extends PPSymbol("<<=")
  case object `>>=` extends PPSymbol(">>=")
  case object `->` extends PPSymbol("->")
  case object `++` extends PPSymbol("++")
  case object `--` extends PPSymbol("--")
  case object `>>` extends PPSymbol(">>")
  case object `<<` extends PPSymbol("<<")
  case object `>=` extends PPSymbol(">=")
  case object `<=` extends PPSymbol("<=")
  case object `*=` extends PPSymbol("*=")
  case object `/=` extends PPSymbol("/=")
  case object `+=` extends PPSymbol("+=")
  case object `%=` extends PPSymbol("%=")
  case object `!=` extends PPSymbol("!=")
  case object `-=` extends PPSymbol("-=")
  case object `&=` extends PPSymbol("&=")
  case object `|=` extends PPSymbol("|=")
  case object `^=` extends PPSymbol("^=")
  case object `[` extends PPSymbol("[")
  case object `]` extends PPSymbol("]")
  case object `)` extends PPSymbol(")")
  case object `(` extends PPSymbol("(")
  case object `}` extends PPSymbol("}")
  case object `{` extends PPSymbol("{")
  case object `.` extends PPSymbol(".")
  case object `&` extends PPSymbol("&")
  case object `*` extends PPSymbol("*")
  case object `/` extends PPSymbol("/")
  case object `%` extends PPSymbol("%")
  case object `|` extends PPSymbol("|")
  case object `+` extends PPSymbol("+")
  case object `-` extends PPSymbol("-")
  case object `~` extends PPSymbol("~")
  case object `!` extends PPSymbol("!")
  case object `?` extends PPSymbol("?")
  case object `:` extends PPSymbol(":")
  case object `,` extends PPSymbol(",")
  case object `=` extends PPSymbol("=")
  case object `;` extends PPSymbol(";")

  def symbol = (
    `<<=`.parser | `>>=`.parser | `->`.parser | `++`.parser | `--`.parser | `>>`.parser |
      `<<`.parser | `>=`.parser | `<=`.parser | `*=`.parser | `/=`.parser | `+=`.parser |
      `%=`.parser | `!=`.parser | `-=`.parser | `&=`.parser | `|=`.parser | `^=`.parser |
      `[`.parser | `]`.parser | `)`.parser | `(`.parser | `}`.parser | `{`.parser | `.`.parser |
      `&`.parser | `*`.parser | `/`.parser | `%`.parser | `|`.parser | `+`.parser | `-`.parser |
      `~`.parser | `!`.parser | `?`.parser | `:`.parser | `,`.parser | `;`.parser | `=`.parser
  )
  case class PPIdent(s: String) extends PPToken
  def digit = PFParser(_.isDigit)
  def nondigit = PFParser(ch => ch.isLower | ch.isUpper | ch == '_')
  def identifier: Parser[PPToken] = nondigit ~ (nondigit | digit).*
    #> {case (a, b) => a +: b.mkString}
    #> {
      case "auto" => PPAuto
      case "double" => PPDouble
      case "int" => PPInt
      case "struct" => PPStruct
      case "break" => PPBreak
      case "else" => PPElse
      case "long" => PPLong
      case "switch" => PPSwitch
      case "case" => PPCase
      case "enum" => PPEnum
      case "register" => PPRegister
      case "typedef" => PPTypedef
      case "char" => PPChar
      case "extern" => PPExtern
      case "return" => PPReturn
      case "union" => PPUnion
      case "const" => PPFloat
      case "float" => PPFloat
      case "short" => PPShort
      case "unsigned" => PPUnsigned
      case "continue" => PPContinue
      case "for" => PPFor
      case "signed" => PPSigned
      case "void" => PPVoid
      case "default" => PPDefault
      case "goto" => PPGoto
      case "sizeof" => PPSizeof
      case "volatile" => PPVolatile
      case "do" => PPDo
      case "if" => PPIf
      case "static" => PPStatic
      case "while" => PPWhile
      case id => PPIdent(id)
    }
  def lineComment =
    '/' ~ '/' ~ PFParser(_ != '\n').* >> WhiteSpace
  def blockComment = '/' ~ '*' ~ (PFParser(_ != '*') | ('*' /> PFParser(_ != '/'))).* ~ '*' ~ '/' >> WhiteSpace
  def comment = lineComment | blockComment
  def wsChar = PFParser(_.isWhitespace) >> WhiteSpace
  def ws: Parser[PPToken] = (wsChar | lineComment | blockComment).+ >> WhiteSpace
  def sign: Parser[Char] = '+' | ElemParser('-')
  def ppNumberTail = (
    '.' >> "."
      | ('e' ~ sign #> { case (a, b) => s"$a$b" })
      | ('E' ~ sign #> { case (a, b) => s"$a$b" })
      | ((digit | nondigit) #> (_.toString))
  )
  def ppNumberStart = (
    ('.' ~ digit #> { case (a, b) => s"$a$b" })
      | (digit #> (_.toString))
  )
  def number: Parser[PPToken] = ppNumberStart ~ ppNumberTail.* #> { case (a, b) => PPNumber(a + b.mkString) }
  def escape = ElemParser('\\') /> (
    (ElemParser('a') >> 0x07.toChar)
      | (ElemParser('b') >> 0x08.toChar)
      | (ElemParser('e') >> 0x1b.toChar)
      | (ElemParser('f') >> 0x0c.toChar)
      | (ElemParser('n') >> 0x0a.toChar)
      | (ElemParser('r') >> 0x0d.toChar)
      | (ElemParser('t') >> 0x09.toChar)
      | (ElemParser('v') >> 0x0b.toChar)
      | (ElemParser('\\') >> 0x5c.toChar)
      | (ElemParser('\'') >> 0x27.toChar)
      | (ElemParser('\"') >> 0x22.toChar)
      | (ElemParser('?') >> 0x3f.toChar)
    // TODO: Hex Escapes
  )
  def charLiteral: Parser[PPToken] = ('\'' /> (escape | PFParser(_ != '\'')).* <\ '\'') #> (ch => PPCharConst(ch.mkString))
  def stringLiteral: Parser[PPToken] =
    ('"' /> (escape | PFParser(_ != '\"')).* <\ '\"') #> (ch => PPString(ch.mkString))
  def token: Parser[Seq[PPToken]] = (ws | identifier | number | charLiteral | stringLiteral | symbol).*

  def parse(input: String): ParseResult[Seq[PPToken]] = {
    token.parseAll(input.toSeq)
  }
}
