package c89.parser
import es.tmoor.parsing.Parsers
import c89.preprocessor.Preprocessor.PPToken
import c89.parser.CParsers.Expression


object CParsers {
  trait Expression
  case class Identifier(name: String) extends Expression
  case class Integer(value: Long) extends Expression
  case class Floating(value: Double) extends Expression
  case class CString(text: String) extends Expression
  case class FunctionCall(fun: Expression, args: Seq[Expression]) extends Expression
  case class DirectMember(obj: Expression, member: Identifier) extends Expression
  case class Dereference(value: Expression) extends Expression
  case class Add(left: Expression, right: Expression) extends Expression
  case class Sub(left: Expression, right: Expression) extends Expression
  case class Mul(left: Expression, right: Expression) extends Expression
  case class Div(left: Expression, right: Expression) extends Expression
  case class Mod(left: Expression, right: Expression) extends Expression
  case class And(left: Expression, right: Expression) extends Expression
  case class Orr(left: Expression, right: Expression) extends Expression
  case class Xor(left: Expression, right: Expression) extends Expression
  case class Not(value: Expression) extends Expression
  case class BNt(value: Expression) extends Expression
}

abstract trait CParsers extends Parsers[PPToken] {
  def expression: Parser[CParsers.Expression]
}
