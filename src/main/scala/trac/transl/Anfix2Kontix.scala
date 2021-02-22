/* This module implements the compilation from Anfix to Kontix */

// TODO : To finish !!

package trac.transl
import scala.collection.immutable.Nil
import trac.anfix.AST.Val
import trac.anfix.AST.Def
import trac.anfix.AST.Simple
import trac.anfix.AST.Let
import trac.anfix.AST.If
import trac.anfix.AST.Op
import trac.anfix.AST.Prim
import trac.anfix.AST.Call
import trac.anfix.AST.Num
import trac.anfix.AST.Str
import trac.anfix.AST.Fun
import trac.anfix.AST.Var

object Anfix2Kontix {

  import trac._
  import trac.anfix.{AST => S}
  import trac.kontix.{AST => T}

  var global_kont: List[T.Definition] = List()

  def trans(p: S.Program): T.Program = {
    T.Program(List(), T.Ret(T.Str("TODO")))
  }

  def compile_definitions(p: S.Program): List[T.Definition] = {
    p match {
      case Nil => List()
      /* Yassine */
      case S.Val(id, e) :: tl => List()
      /* Richard */
      case S.Def(fid, args, e) :: tl => List()
    }
  }

  def compile_expr_to_tail(e: S.Expr): T.TailExpr = {
    e match {
      /* pas sur */
      case S.Simple(e) => T.Ret(compile_simple_to_basic(e))
      /* peut optimiser peut être simple */
      /* Yassine */
      case S.Let(id, e1, e2) => T.Ret(T.Num(0))
      case S.If(c, e2, e3)   => T.Ret(T.Num(0))
      case S.Op(o, e1, e2)   => T.Ret(T.Num(0))

      /* Richard */
      case S.Prim(o, args) => T.Ret(T.Num(0))
      /* Richard */
      case S.Call(f, args) => T.Ret(T.Num(0))
    }
  }

  def compile_simple_to_basic(e: S.SimplExpr): T.BasicExpr = {
    e match {
      case S.Num(n)   => T.Num(n)
      case S.Str(s)   => T.Str(s)
      case S.Fun(fid) => T.Fun(fid)
      case S.Var(id)  => T.Var(id)
    }
  }

}
