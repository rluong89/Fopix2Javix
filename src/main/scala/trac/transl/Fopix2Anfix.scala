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

object Fopix2Anfix {

  import trac._
  import trac.fopix.{AST => S}
  import trac.anfix.{AST => T}

  def trans(p: S.Program): T.Program = {
    p match {
      case Nil                       => List()
      case S.Val(id, e) :: tl        => List()
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
      case Op(o, e1, e2) => T.Simple(T.Num(0)) /* Richard */
      case Prim(p, args) => T.Simple(T.Num(0)) /* Richard */
      case Call(f, args) =>
        T.Simple(T.Num(0)) /* Yassine indirect Richard direct */
    }
  }

}
