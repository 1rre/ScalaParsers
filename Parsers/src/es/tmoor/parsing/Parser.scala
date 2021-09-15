package es.tmoor.parsing

type ParseResult[T] = NoMatch.type | Match[T]
case object NoMatch
case class Match[T](idx: Int, result: T) {
  override def toString = s"Match: $result"
}

abstract class Parser[T] {
  def parseAll(input: String, from: Int = 0): ParseResult[T] = {
    parse(input, from) match {
      case Match(idx, result: T) if idx == input.length => Match(idx, result)
      case _ => NoMatch
    }
  }
  def parse(input: String, from: Int = 0): ParseResult[T]
  def ~[T1](other: => Parser[T1]): CombiParser[T,T1] = new CombiParser(this, other)
  def |[T1 >: T](other: => Parser[T1]): ChoiceParser[T,T1] = new ChoiceParser(this, other)
  def #>[T1](transform: PartialFunction[T,T1]): Parser[T1] = new TransformParser(this, transform)
  def #>[T1](transform: T => T1): Parser[T1] = new TransformParser(this, transform)
}
class TransformParser[T1,T2](parseIn: Parser[T1], transform: PartialFunction[T1,T2]) extends Parser[T2] {
  def this(parseIn: Parser[T1], transform: T1 => T2) = this(parseIn, {case x => transform(x)})
  def parse(input: String, from: Int): ParseResult[T2] = {
    parseIn.parse(input, from) match {
      case NoMatch => NoMatch
      case Match(idx, result: T1) => Match(idx, transform(result))
    }
  }
}
class CharParser(c: Char) extends Parser[Char] {
  def parse(input: String, from: Int): ParseResult[Char] = {
    if (input.length <= from) NoMatch
    else if (input(from) == c) Match(from + 1, c)
    else NoMatch
  }
}
class PFParser(pf: Char => Boolean) extends Parser[Char] {
  def this(pf: PartialFunction[Char, Boolean]) = this(ch => pf.applyOrElse(ch, (_: Any) => false))
  def parse(input: String, from: Int): ParseResult[Char] = {
    if (input.length <= from) NoMatch
    else if (pf(input(from))) Match(from + 1, input(from))
    else NoMatch
  }
}
class ChoiceParser[T1,T2 >: T1](left: => Parser[T1], right: => Parser[T2]) extends Parser[T2] {
  def parse(input: String, from: Int): ParseResult[T2] = {
    left.parse(input, from) match {
      case NoMatch => right.parse(input, from)
      case result => result.asInstanceOf[ParseResult[T2]]
    }
  }
}
class CombiParser[T1,T2](left: => Parser[T1], right: => Parser[T2]) extends Parser[(T1,T2)] {
  def parse(input: String, from: Int): ParseResult[(T1,T2)] = {
    left.parse(input, from) match {
      case Match(i1, r1) =>
        right.parse(input, i1) match {
          case Match(i2, r2) => Match(i2, (r1,r2))
          case NoMatch => NoMatch
        }
      case NoMatch => NoMatch
    }
  }
}