/* This module implements a translation from Fopix to Anfix */

// TODO : To finish !!

package trac.transl
import scala.collection.immutable.Nil
import trac.fopix.AST.Num
import trac.fopix.AST.Str
import trac.fopix.AST.Fun
import trac.fopix.AST.Var
import trac.fopix.AST.Let
import trac.fopix.AST.If
import trac.fopix.AST.Op
import trac.fopix.AST.Prim
import trac.fopix.AST.Call
import trac.anfix.AST.Simple
import trac.anfix.AST

object Fopix2Anfix {

  import trac._
  import trac.fopix.{AST => S}
  import trac.anfix.{AST => T}

  final case class CustomException(private val message : String = "", private val cause : Throwable = None.orNull)
  extends Exception(message, cause)

  def trans(p: S.Program): T.Program = {
    p match {
      case Nil                       => List()
      case S.Val(id, e) :: tl        => 
        List(T.Val(id, trans_expr(e))) ++ trans(tl)
      case S.Def(fid, args, e) :: tl => 
        List(T.Def(fid, args, trans_expr(e))) ++ trans(tl)
    }
  }
  var _count = 0

  def generateLabel(): String = {
    val new_label = "x" + _count
    _count += 1
    new_label
  }

  def prim(args : List[S.Expr], acc : List[T.SimplExpr], p : PrimOp.T) : T.Expr = {
    args match {
      case Nil => T.Prim(p, acc)
      case x :: xs => 
        val trans_x = trans_expr(x) 
        trans_x match {
          case T.Simple(e) => prim(xs, acc ++ List(e), p)
          case x => 
            val id = generateLabel()
            T.Let(id, trans_x, prim(xs, acc ++ List(T.Var(id)), p))
        }
    }
  }

  def call(args : List[S.Expr], acc : List[T.SimplExpr], f : T.SimplExpr) : T.Expr = {
    args match {
      case Nil => T.Call(f, acc)
      case x :: xs => 
        val trans_x = trans_expr(x) 
        trans_x match {
          case T.Simple(e) => call(xs, acc ++ List(e), f)
          case x => 
            val id = generateLabel()
            T.Let(id, trans_x, call(xs, acc ++ List(T.Var(id)), f))
        }
    }
  }

  def trans_expr(e: S.Expr): T.Expr = {
    e match {
      case Num(n)   => T.Simple(T.Num(n))
      case Str(s)   => T.Simple(T.Str(s))
      case Fun(fid) => T.Simple(T.Fun(fid))
      case Var(id)  => T.Simple(T.Var(id))
      case Let(id, e1, e2) =>
        T.Let(id, trans_expr(e1), trans_expr(e2)) /* Yassine */
      case If(e1, e2, e3) =>
        val new_id = generateLabel()
        T.Let(
          new_id,
          trans_expr(e1),
          T.If(
            /* Pas sur ptet 1 */
            (BinOp.toCmp(BinOp.Eq), T.Var(new_id), T.Num(0)),
            trans_expr(e2),
            trans_expr(e3)
          )
        ) /* Yassine */
      case Op(o, e1, e2) => 
        val trans_e1 = trans_expr(e1)
        val trans_e2 = trans_expr(e2)
        (trans_e1, trans_e2) match {
          case (T.Simple(se1), T.Simple(se2)) => 
            T.Op(BinOp.toArith(o), se1, se2)
          case (T.Simple(se), trans_e2) =>
            val id = generateLabel() 
            val operation = T.Op(BinOp.toArith(o), se, T.Var(id))
            T.Let(id, trans_e2, operation)
          case (trans_e1, T.Simple(se)) =>
            val id = generateLabel() 
            val operation = T.Op(BinOp.toArith(o), se, T.Var(id))
            T.Let(id, trans_e1, operation)  
          case (trans_e1, trans_e2) =>
            val id1 = generateLabel()
            val id2 = generateLabel()
            val operation = T.Op(BinOp.toArith(o), T.Var(id1), T.Var(id2))
            val let2 = T.Let(id2, trans_e2, operation)
            T.Let(id1, trans_e1, let2)
          case _ => throw CustomException("to do")
        }
      case Prim(p, args) => 
        prim(args, List[T.SimplExpr](), p)
      case Call(f, args) =>
        val trans_f = trans_expr(f)
        trans_f match {
          case Simple(se) => call(args, List[T.SimplExpr](), se)
          case _ => throw CustomException("Impossible")
        }
    }
  }

}
