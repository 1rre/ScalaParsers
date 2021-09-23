import util.parsing.input.CharSequenceReader
import collection.mutable.Buffer
import ir.types._
import mips._
import ir.Optimiser

object Main extends App {
  val inStr = (
"""
pub main @(4)[i32] (
  r1 <= i32[1]
  r2 <= i32[0]
  s0 <= r2
  r1 <= @[s0]
  call addThree
  @[r1] <= r0
  r0 <= s0
)
fun addThree (
  call +
  r1 <= r2
  call +
)
"""
  )
  val input = new CharSequenceReader(inStr)
  val parseRes = (ir.Parser.fun.*)(input).getOrElse(Nil)
  println(parseRes)
  val optimiser = new Optimiser(parseRes)
  val optRes = optimiser.optimise()
  for (gs <- optRes) {
    println(gs)
  }
}
