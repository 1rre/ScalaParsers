import es.tmoor.parsing._

object Test extends App with Parsers[Char] {
  def variable: Parser[String] = (
    (PFParser(_.isLower) ~ variable) #> {case (a,b) => a +: b} |
    (PFParser(_.isLower) #> (_.toString))
  )
  def digitSeq: Parser[String] = (
    (PFParser(_.isDigit) ~ digitSeq #> {case (a,b) => a +: b}) |
    (PFParser(_.isDigit) #> (_.toString))
  )
  def number: Parser[Int] = (
    (ElemParser('-') ~ digitSeq #> (-_._2.toInt)) |
    (digitSeq #> (_.toInt))
  )
  type Operand = String | Int | Operation
  abstract class Operation(left: Operand, right: Operand)
  class Add(left: Operand, right: Operand) extends Operation(left, right) {
    override def toString = s"($left + $right)"
  }
  class Sub(left: Operand, right: Operand) extends Operation(left, right) {
    override def toString = s"($left - $right)"
  }
  class Mul(left: Operand, right: Operand) extends Operation(left, right) {
    override def toString = s"($left * $right)"
  }
  def ws: Parser[Char] = ElemParser(' ')


  def exprBracket: Parser[Operand] =
    ElemParser('(') ~ expression ~ ElemParser(')')
      #> (_._1._2)
  def exprLiteral: Parser[Operand] =
    variable.asInstanceOf[Parser[Operand]]
    | number.asInstanceOf[Parser[Operand]]
    | exprBracket
  def exprMul: Parser[Operand] = 
    (exprLiteral ~ ws ~ ElemParser('*') ~ ws ~ exprMul
      #> {case ((((a,_),_),_),b) => Mul(a,b)})
    | exprLiteral
  def exprSub: Parser[Operand] =
    (exprMul ~ ws ~ ElemParser('-') ~ ws ~ exprAdd
      #> {case ((((a,_),_),_),b) => Sub(a,b)})
    | exprMul
  def exprAdd: Parser[Operand] =
    (exprSub ~ ws ~ ElemParser('+') ~ ws ~ exprAdd
      #> {case ((((a,_),_),_),b) => Add(a,b)})
    | exprSub
  def expression: Parser[Operand] = exprAdd

  val input = "(5 + -3 * 5) + 2 * 3 + acb * abcv - (5 * d)"
  val result = expression.parseAll(input)
  println(result)
}
