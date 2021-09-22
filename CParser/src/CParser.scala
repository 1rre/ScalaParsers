import es.tmoor.parsing._
object CParser {
  /*
  def digit = PFParser(_.isDigit)
  def nondigit = PFParser(ch => ch.isLower | ch.isUpper | ch == '_')
  def identifier: Parser[String] =
    nondigit ~ (nondigit | digit).* #> {case (a,b) => a +: b.mkString}

  // TODO: numeric
  def nonZero = PFParser(ch => ch != '0' && ch.isDigit)
  def intConstant10 = nonZero ~ digit.* #> {case (a,b) => (a+:b).mkString.toInt}

  def escape = CharParser('\\') /> (
    (CharParser('a')   >> 0x07.toChar)
    | (CharParser('b') >> 0x08.toChar)
    | (CharParser('e') >> 0x1B.toChar)
    | (CharParser('f') >> 0x0C.toChar)
    | (CharParser('n') >> 0x0A.toChar)
    | (CharParser('r') >> 0x0D.toChar)
    | (CharParser('t') >> 0x09.toChar)
    | (CharParser('v') >> 0x0B.toChar)
    | (CharParser('\\')>> 0x5C.toChar)
    | (CharParser('\'')>> 0x27.toChar)
    | (CharParser('\"')>> 0x22.toChar)
    | (CharParser('?') >> 0x3F.toChar)
    // TODO: Hex Escapes
  )

  def char = CharParser('\'') /> (escape | PFParser(_ != '\'')) <\ CharParser('\'')

  def constant: Parser[Int] = intConstant10 | (char #> (_.intValue))

  def stringLiteral = CharParser('"') /> ((escape | PFParser(_ != '"')).* #> (_.mkString)) <\ CharParser('"')
  trait PostfixExpression
  case class PostfixOffset(value: Expression, offset: Expression) extends PostfixExpression
  case class StructMember(struct: Expression, member: String) extends PostfixExpression
  case class FunctionCall(calledOn: Expression, args: Seq[Expression]) extends PostfixExpression
  type Expression = Int | String | PostfixExpression

  def expression = postfixExpr
  def primaryExpr: Parser[Expression] = (
    identifier.asInstanceOf[Parser[Expression]]
    | constant.asInstanceOf[Parser[Expression]]
    | stringLiteral.asInstanceOf[Parser[Expression]]
    | (CharParser('(') /> expression <\ CharParser(')'))
  )
  class PfTail(fun: Expression => PostfixExpression) {
    def apply(expr: Expression): Expression = fun(expr)
  }
  case class PfOffsetTail(offset: Expression) extends PfTail(PostfixOffset(_, offset))
  def pfOffset: Parser[PfTail] = (CharParser('[') /> expression <\ CharParser(']')) #> (PfOffsetTail(_))
  case class PfStructTail(id: String) extends PfTail(StructMember(_, id))
  def pfStruct: Parser[PfTail] = CharParser('.') /> identifier #> (PfStructTail(_))
  case class PfFunCall(args: Seq[Expression]) extends PfTail(FunctionCall(_, args))
  def pfFunCall: Parser[PfTail] = (CharParser('(') /> expression.*(CharParser(',')) <\ CharParser(')')) #> (PfFunCall(_))
  def pfTail: Parser[PfTail] = (
    pfOffset
    | pfStruct
    | pfFunCall
  )
  def postfixExpr = (
    primaryExpr ~ pfTail.* #> {case (a,b) => b.foldLeft(a)((acc,v) => v(acc))}
  )
  */
}