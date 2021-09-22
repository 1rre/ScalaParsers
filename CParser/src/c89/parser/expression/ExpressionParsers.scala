package c89.parser
package expression
import c89.preprocessor.Preprocessor._

class ExpressionParsers extends PostfixExpressionParsers {
  def expression: Parser[Expression] = (
    postfixExpression
  )
}