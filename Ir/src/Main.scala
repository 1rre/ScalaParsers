import util.parsing.input.CharSequenceReader
import collection.mutable.Buffer
import mips._
import ir._
import ir.types._

object Main extends App {
  val inStr = (
"""
ext fun[i32] i32 i32 f
@[fun[i32] i32 i32] fPtr @[f]
ext obj[i32, obj[i32, i8, u8, obj[f64, @[fun[i32]]]]] x
fun[i32] @(4)[i32] main (
  r1 <= i32[1]
  r2 <= i32[0]
  s0 <= r2
  r1 <= @[s0]
  r2 <= @[addThree]
  @[r2]
  @[r1] <= r0
  r0 <= s0
  f
)
fun[i32] i32 i32 i32 addThree (
  +
  r1 <= r2
  +
)
"""
  )
  val input = new CharSequenceReader(inStr)
  val parseRes = ir.Parser.global(input)
  println(parseRes)
  println()
  //val optimiser = new Optimiser(parseRes.getOrElse(Nil))
  //val optRes = optimiser.optimise()
  parseRes.getOrElse(Nil).foreach {
    case decl: Decl => {
      println(s"Decl: ${decl.name}")
      println(s"Type: ${decl.ofType}")
      println()
    }
    case ext: Ext => {
      println(s"External: ${ext.name}")
      println(s"Type: ${ext.ofType}")
      println()
    }
  }
}
