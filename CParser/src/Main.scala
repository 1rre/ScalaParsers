import c89.preprocessor.Preprocessor
import c89.parser.expression.ExpressionParsers
//import CParser._

object Main extends App {

  val input = (
"""
 "14232314"[5][5].run(1,2,3,4,5) -> hello()
""")
  val ppR =
    Preprocessor.parse(input) match
      case Preprocessor.NoMatch(idx) =>
        val lines = input.split('\n').scanLeft(("",-1))((acc, v) => (v, acc._2+v.size+1)).tail
        val errorLine = lines.indexWhere(_._2 > idx)
        val errorPos = lines(errorLine)._1.length - lines(errorLine)._2 + idx
        Console.err.println(s"Error at ${errorLine+1}:${errorPos+1}")
        Console.err.println(lines(errorLine)._1)
        Console.err.println(Array.fill(errorPos)(' ').mkString :+ '^')
        System.exit(0)
        Nil
      case Preprocessor.Match(idx, result) =>
        result.filterNot(_ == Preprocessor.WhiteSpace)

  println(ppR)
  println
  val cp = new ExpressionParsers
  val cpR = cp.expression.parseAll(ppR)
  println(cpR)

}