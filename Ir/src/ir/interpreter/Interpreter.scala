package ir.interpreter

import ir._

class Interpreter(program: Seq[GlobalStatement]) {
  for (Ext(name, _) <- program)
    sys.error(s"""Cannot interpret external "$name"""")

}
