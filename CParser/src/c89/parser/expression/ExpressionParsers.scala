package c89.parser
package expression
import c89.preprocessor.Preprocessor._
import CParsers._

class ExpressionParsers extends UnaryExpressionParsers {
  def expression: Parser[Expression] = (
    unaryExpression
  )
}