package c89.parser
import es.tmoor.parsing.Parsers
import c89.preprocessor.Preprocessor.PPToken

abstract trait CParsers extends Parsers[PPToken] {
  trait Expression
  def expression: Parser[Expression]
}
