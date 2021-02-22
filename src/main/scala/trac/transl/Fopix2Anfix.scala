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
      case S.Def(fid, args, e) :: tl => List()
    }
  }
  var _count = 0

  def generateLabel(): String = {
    val new_label = "x" + _count
    _count += 1
    new_label
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
        val id1 = generateLabel()
        val id2 = generateLabel()
        T.Let(id2, trans_expr(e2), T.Let(id1, trans_expr(e1), T.Op(BinOp.toArith(o), T.Var(id1), T.Var(id2))))
      case Prim(p, args) => 
        val compiled_args = args.foldLeft(List[T.SimplExpr]()) {
          (acc, elt) =>
            val trans = trans_expr(elt)
            trans match {
              case T.Simple(e) => acc ++ List(e)
              case T.Let(_, _, Simple(e)) => acc ++ acc
              case _ => throw CustomException("to do or fail")
            }
        }
      T.Prim(p, compiled_args)
      case Call(f, args) =>
        T.Simple(T.Num(0)) /* Yassine indirect Richard direct */
    }
  }

}
