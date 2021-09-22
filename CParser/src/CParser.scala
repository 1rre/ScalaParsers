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
  sealed class PFTail(fun: Expression => Expression) {
    def apply(expr: Expression) = fun(expr)
  }
  case class PFOffsetTail(offset: Expression) extends PFTail(expr => PFOffset(expr, offset))
  case class PFOffset(value: Expression, offset: Expression) extends Expression
  def postfixOffsetTail: Parser[PFTail] = 
    (`[` /> expression <\ `]`) #> (PFOffsetTail(_))
  def pfTail: Parser[PFTail] = (
    postfixOffsetTail
  )
  def postfixExpression: Parser[Expression] = (
    primaryExpression ~ pfTail.* #> {case (a,b) => b.foldLeft(a)((acc,v) => v(acc))}
  )
  def expression: Parser[Expression] = (
    postfixExpression
  )
}
