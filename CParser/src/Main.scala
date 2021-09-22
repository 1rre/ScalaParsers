import CParser._
import Preprocessor._

object Main extends App {

  val input = (
"""
int float

float int double struct
/* asdkjljgsadlasdfgkhdsfa
jg+lkd struct */
// aslfjkgdsjgagf fs j
struct
""")
  val result = token.parseAll(input.toSeq)
  println(result)
}