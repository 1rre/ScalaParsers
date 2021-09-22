package c89.parser.expression
import c89.preprocessor.Preprocessor._

abstract trait PostfixExpressionParsers extends PrimaryExpressionParsers {
  sealed class PFTail(fun: Expression => Expression) {
    def apply(expr: Expression) = fun(expr)
  }

  case class PFOffset(value: Expression, offset: Expression) extends Expression
  case class PFOffsetTail(offset: Expression) extends PFTail(expr => PFOffset(expr, offset))
  def pfOffsetTail: Parser[PFTail] = `[` /> expression #> (PFOffsetTail(_)) <\ `]`

  case class PFFunCall(value: Expression, args: Seq[Expression]) extends Expression
  case class PFFunCallTail(args: Seq[Expression]) extends PFTail(expr => PFFunCall(expr, args))
  def pfFunCallTail: Parser[PFTail] = `(` /> expression.*(`,`) #> (PFFunCallTail(_)) <\ `)`

  case class PFSMember(struct: Expression, member: Identifier) extends Expression
  case class PFSMemberTail(member: Identifier) extends PFTail(expr => PFSMember(expr, member))
  def pfSMemberTail: Parser[PFTail] = `.` /> primaryIdentifier #> (PFSMemberTail(_))

  case class PFSAccess(struct: Expression, member: Identifier) extends Expression
  case class PFSAccessTail(member: Identifier) extends PFTail(expr => PFSAccess(expr, member))
  def pfSAccessTail: Parser[PFTail] = `->` /> primaryIdentifier #> (PFSAccessTail(_))

  def pfTail: Parser[PFTail] = (
    pfFunCallTail
    | pfOffsetTail
    | pfSMemberTail
    | pfSAccessTail
  )
  def postfixExpression: Parser[Expression] = (
    primaryExpression ~ pfTail.* #> {case (a,b) => b.foldLeft(a)((acc,v) => v(acc))}
  )
}