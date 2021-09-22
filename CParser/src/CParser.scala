import es.tmoor.parsing._
import Preprocessor._
import CParser.Expression
object CParser extends Parsers[PPToken] {
  trait Expression
  case class Identifier(text: String) extends Expression
  case class Number(text: String) extends Expression
  case class CChar(text: String) extends Expression
  case class CString(text: String) extends Expression
  def primaryIdentifier: Parser[Expression] =
    (PFParser({ case _: PPIdent => true })
      #> { case id: PPIdent => Identifier(id.s) })
  def primaryNumber: Parser[Expression] =
    (PFParser({ case _: PPNumber => true })
      #> { case n: PPNumber => Number(n.s) })
  def primaryChar: Parser[Expression] =
    (PFParser({ case _: PPCharConst => true })
      #> { case c: PPCharConst => CChar(c.s) })
  def primaryString: Parser[Expression] =
    (PFParser({ case _: PPString => true })
      #> { case s: PPString => CString(s.s) })
  def primaryExpression: Parser[Expression] = (
    primaryIdentifier
      | primaryNumber
      | primaryChar
      | primaryString
      | (`(` /> expression <\ `)`)
  )
  def expression: Parser[Expression] = (
    primaryExpression
  )
}
