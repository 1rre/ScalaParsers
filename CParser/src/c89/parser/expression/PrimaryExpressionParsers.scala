package c89.parser
package expression
import c89.preprocessor.Preprocessor._
import CParsers._

abstract trait PrimaryExpressionParsers extends CParsers {
  def primaryIdentifier =
    TypeParser[PPIdent] #> { case PPIdent(s) => Identifier(s) }
  def primaryNumber: Parser[Expression] =
    TypeParser[PPNumber] #> {
      case n: PPNumber if (n.s.toIntOption.isDefined) =>
        Integer(n.s.toInt)
      case n: PPNumber if (n.s.toDoubleOption.isDefined) =>
        Floating(n.s.toDouble)
    }
  def primaryChar: Parser[Expression] =
    TypeParser[PPCharConst] #> {
      case c: PPCharConst => Integer(c.s.foldLeft(0)(_*256+_))
    }
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
