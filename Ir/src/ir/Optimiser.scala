package ir

import ir.value._
import collection.mutable.Buffer
import collection.mutable.Set

object Optimiser {
  type Program = Seq[GlobalStatement]
}
import Optimiser._

class Optimiser(program: Program) {
  val funs = program.collect { case f: Fun =>
    f
  }.toArray
  val namedFuns = funs.zipWithIndex.collect { case (fn: DeclaredFun, i: Int) =>
    fn.name -> i
  }.toMap

  def resolveFunCalls(exprs: Seq[Expression]): Seq[Expression] = {
    exprs.map {
      case CallUnresolved(name) => new FunCallDelayed(funs(namedFuns(name)))
      case expr => expr
    }
  }
  def countUsage(fun: DeclaredFun): (Int, Int, Int) = {
    val stack = collection.mutable.Set[Int]()
    def nStack = stack.maxOption.getOrElse(0)
    val args = collection.mutable.Set[Int]()
    def nArgs = args.maxOption.getOrElse(0)
    val local = collection.mutable.Set[Int]()
    def nLocal = local.maxOption.getOrElse(0)
    def useNReg(n: Int): Unit = {
      for (i <- 1 to n) {
        if (!local.contains(i)) args += i
        local += i
      }
    }
    for (expr <- fun.exprs) {
      println(s"Analysing: $expr")
      expr match {
        case fc: CallResolved if fc.called.isInstanceOf[HasArgs] =>
          useNReg(fc.called.asInstanceOf[HasArgs].nArgs)
        case Copy(Local(rd), Local(rs)) => {
          if (!local.contains(rs + 1)) args += rs + 1
          local += rs + 1
          local += rd + 1
        }
        case Copy(Local(rd), _) => {
          local += rd + 1
        }
        case Copy(_, Local(rs)) => {
          if (!local.contains(rs + 1)) args += rs
          local += rs + 1
        }
        case _: Copy =>
        case _: Call => 
        // Either undefined function or recursive
      }
      println(s"Usage: Args: $nArgs, Local: $nLocal, Stack: $nStack")
    }
    (nArgs, nLocal, nStack)
  }
  def reOptFun(of: OptFun): OptFun = {
    of
  }
  def optLocalFun(uf: UnoptLocalFun): OptFun = {
    val newExprs = resolveFunCalls(uf.exprs)
    val (nArgs, nReg, nStack) = countUsage(UnoptLocalFun(uf.name, newExprs))
    OptFun(uf.name, newExprs, nArgs, nReg, nStack)
  }
  def optPublishedFun(uf: UnoptPublishedFun): OptFun = {
    val newExprs = resolveFunCalls(uf.exprs)
    val (nArgs, nReg, nStack) = countUsage(UnoptPublishedFun(uf.name, uf.args, newExprs))
    OptFun(uf.name, newExprs, nArgs, nReg, nStack)
  }
  private def optimiseFun(f: Fun): Fun = {
    f match {
      case of: OptFun => reOptFun(of)
      case uf: UnoptLocalFun => optLocalFun(uf)
      case uf: UnoptPublishedFun => optPublishedFun(uf)
      case nf: InternalFun => nf
      case ef: ExternalFun => ef
    }
  }
  def optimise(): Program = {
    funs.mapInPlace(optimiseFun(_)).toIndexedSeq
  }
}
