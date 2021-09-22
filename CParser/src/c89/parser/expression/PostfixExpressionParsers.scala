package c89.parser.expression
import c89.preprocessor.Preprocessor._
import c89.parser.CParsers._

abstract trait PostfixExpressionParsers extends PrimaryExpressionParsers {
  sealed class PFTail(fun: Expression => Expression) {
    def apply(expr: Expression) = fun(expr)
  }
  case class PFOffsetTail(offset: Expression) extends PFTail(expr => Dereference(Add(expr, offset)))
  def pfOffsetTail: Parser[PFTail] = `[` /> expression #> (PFOffsetTail(_)) <\ `]`

  case class PFFunCallTail(args: Seq[Expression]) extends PFTail(expr => FunctionCall(expr, args))
  def pfFunCallTail: Parser[PFTail] = `(` /> expression.*(`,`) #> (PFFunCallTail(_)) <\ `)`

  case class PFSMemberTail(member: Identifier) extends PFTail(expr => DirectMember(expr, member))
  def pfSMemberTail: Parser[PFTail] = `.` /> primaryIdentifier #> (PFSMemberTail(_))

  case class PFSAccessTail(member: Identifier) extends PFTail(expr => DirectMember(Dereference(expr), member))
  def pfSAccessTail: Parser[PFTail] = `->` /> primaryIdentifier #> (PFSAccessTail(_))

  case object PFIncrementTail extends PFTail(expr => Add(expr, Integer(1)))
  def pfIncrementTail: Parser[PFTail] = `++` >> PFIncrementTail

  case object PFDecrementTail extends PFTail(expr => Sub(expr, Integer(1)))
  def pfDecrementTail: Parser[PFTail] = `--` >> PFDecrementTail

  def pfTail: Parser[PFTail] = (
    pfFunCallTail
      | pfOffsetTail
      | pfSMemberTail
      | pfSAccessTail
      | pfIncrementTail
      | pfDecrementTail
  )
  def postfixExpression: Parser[Expression] = (
    primaryExpression ~ pfTail.* #> { case (a, b) => b.foldLeft(a)((acc, v) => v(acc)) }
  )
}
