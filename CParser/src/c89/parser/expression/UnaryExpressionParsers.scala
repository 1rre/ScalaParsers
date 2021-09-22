package c89.parser.expression
import c89.preprocessor.Preprocessor._
import c89.parser.CParsers._

abstract trait UnaryExpressionParsers extends PostfixExpressionParsers {
  sealed class UInit(fun: Expression => Expression) {
    def apply(expr: Expression) = fun(expr)
  }

  case object UIncrementInit extends UInit(expr => Add(expr, Integer(1)))
  def uIncrementInit: Parser[UInit] = `++` >> UIncrementInit

  case object UDecrementInit extends UInit(expr => Sub(expr, Integer(1)))
  def uDecrementInit: Parser[UInit] = `--` >> UDecrementInit

  def uInit: Parser[UInit] = (
    uIncrementInit
    | uDecrementInit
  )

  def unaryExpression: Parser[Expression] = (
    uInit.* ~ postfixExpression #> { case (b, a) => b.foldLeft(a)((acc, v) => v(acc)) }
  )
}