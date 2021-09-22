import util.parsing.input.CharSequenceReader

object Main extends App {
  val input = new CharSequenceReader("obj[32#u32, 32#s64, 36.0#f32, 123#>5(f32)]")
  val res = IRParser.constant(input)
  println(res)
}