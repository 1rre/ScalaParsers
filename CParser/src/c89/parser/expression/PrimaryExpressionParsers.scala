package c89.parser
package expression
import c89.preprocessor.Preprocessor._

abstract trait PrimaryExpressionParsers extends CParsers {
  case class Identifier(text: String) extends Expression
  case class Number(text: String) extends Expression
  case class CChar(text: String) extends Expression
  case class CString(text: String) extends Expression
  def primaryIdentifier =
    TypeParser[PPIdent] #> { case PPIdent(s) => Identifier(s) }
  def primaryNumber: Parser[Expression] =
    TypeParser[PPNumber] #> { case n: PPNumber => Number(n.s) }
  def primaryChar: Parser[Expression] =
    TypeParser[PPCharConst] #> { case c: PPCharConst => CChar(c.s) }
  def primaryString: Parser[Expression] =
    TypeParser[PPString] #> { case s: PPString => CString(s.s) }
  def primaryExpression: Parser[Expression] = (
    primaryIdentifier
      | primaryNumber
      | primaryChar
      | primaryString
      | (`(` /> expression <\ `)`)
  )
}
