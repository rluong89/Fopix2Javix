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
import trac.kontix.AST

object Anfix2Kontix {

  import trac._
  import trac.anfix.{AST => S}
  import trac.kontix.{AST => T}

  var global_kont: List[T.Definition] = List()

  def trans(p: S.Program): T.Program = {
    val immuetableList = List()
    val (defs, tailexpr) = compile_definitions(p, immuetableList)
    println(tailexpr)
    T.Program(defs, tailexpr)
  }

  var _count = 0

  def generateLabel(): String = {
    val new_label = "x" + _count
    _count += 1
    new_label
  }

  def compile_definitions(
      p: S.Program,
      nestedEnv: List[String]
  ): (List[T.Definition], T.TailExpr) = {
    p match {
      case Nil => (List(), T.Ret(T.Num(0)))
      /* Yassine */
      case S.Val(id, e) :: tl =>
        //Liste immutable ?
        val new_list = if (id.equals("_")) { nestedEnv }
        else { id :: nestedEnv }
        val recursive_res = compile_definitions(tl, new_list)
        e match {
          case S.Simple(e) =>
            (
              recursive_res._1,
              T.Let(id, compile_simple_to_basic(e), recursive_res._2)
            )
          case e =>
            /* Faire une continuation */
            val generatedLabel = generateLabel()
            val tailexpr = compile_expr_to_tail(e)
            val defkont = T.DefCont(generatedLabel, nestedEnv, id, tailexpr)
            val pushkont =
              T.PushCont(generatedLabel, nestedEnv, recursive_res._2)
            val new_definitions = defkont :: recursive_res._1
            val new_tailexpr = pushkont
            (new_definitions, new_tailexpr)
        }

      /* Richard */
      case S.Def(fid, args, e) :: tl =>
        val recursive_res = compile_definitions(tl, nestedEnv)
        recursive_res
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
      case S.Op(o, e1, e2) =>
        val eg = compile_simple_to_basic(e1)
        val ed = compile_simple_to_basic(e2)
        T.Ret(T.Op(o, eg, ed))
      /* Richard */
      case S.Prim(o, args) => handlePrim(o, args)
      case S.Call(f, args) => T.Ret(T.Num(0))
    }
  }

  def handlePrim(o: PrimOp.T, args: List[anfix.AST.SimplExpr]): T.TailExpr = {
    o match {
      case PrimOp.Printint =>
        val liste = args.map(e => compile_simple_to_basic(e))
        T.Ret(T.Prim(o, liste))
      case _ => T.Ret(T.Num(0))
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
