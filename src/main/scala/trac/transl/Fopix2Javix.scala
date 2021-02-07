/* This module implements a compiler from Fopix to Javix. */

// TODO : To finish !!

package trac.transl

import java.util.UUID
import trac.javix.AST.ALoad

object Fopix2Javix {

  import trac._
  import trac.PrimOp._
  import trac.BinOp._
  import trac.fopix.{AST => S}
  import trac.javix.{AST => T}

  /* /!\ Attention, le code suivant n'est qu'une suggestion pour commencer.
   * Il est probablement à remanier encore (structures en plus, arguments
   * supplémentaires, ...) */

  type Env = Map[S.Ident, Int]

  def computeVarSize(instrs: List[T.Instruction]): Int = {
    val pair = instrs.foldLeft((0, 0))((acc, elt) => {
      elt match {
        case T.AStore(_) => (acc._1 + 1, acc._2)
        case T.ALoad(_)  => (acc._1, acc._2 + 1)
        case _           => acc
      }
    })
    Math.max(pair._1, pair._2)
  }

  def compile(progname: String, p: S.Program): T.Program = {
    val stacksize = 10000
    //val instrs = List(T.Comment("Todo!!"),T.Return)
    val env: Env = Map.empty
    val instrs = compile_definitions(p, env) ++ List(T.Return)
    val varsize = computeVarSize(instrs)
    T.Program(progname, instrs, varsize, stacksize)
  }

  var count = 0

  def compile_definitions(
      p: List[S.Definition],
      env: Env
  ): List[T.Instruction] = {
    /* TODO: à completer, on ne s'occupe ici que du premier Val ! */
    p match {
      case Nil => List()
      case S.Val(x, e) :: p =>
        val instructions = compile_expr(e, env)
        val optionX = env get (x)
        val (newInstruction, new_env) =
          optionX match {
            case Some(value) => (List(T.AStore(value)), env)
            case None =>
              if (x.equals("_")) {
                (List(), env)
              } else {
                count += 1
                (List(T.AStore(count)), (env + (x -> count)))
              }
          }
        instructions ++ newInstruction ++
          (compile_definitions(p, new_env))

      case S.Def(_, _, _) :: p => /* Richard appel direct */
        compile_definitions(p, env)
    }
  }

  /* TODO: ajouter une structure d'environnement des variables,
   * du genre Map[S.Ident,T.Var] donnant le numéro de la variable Javix
   * correspondant à un nom de variable Fopix */

  /* Invariant :
   *  A l'exécution, le code généré par compile_expr(e) aura l'effet suivant :
   *    pile ==> pile,v  avec v la valeur résultat de l'évaluation de e
   */

  def generateLabel(s: String): String = {
    s + "_" + UUID.randomUUID().toString
  }

  def compile_expr(e: S.Expr, env: Env): List[T.Instruction] = {
    e match {
      case S.Num(n) => List(T.Push(n), T.Box)
      case S.Str(s) => List(T.Ldc(s))
      case S.Var(v) => List(T.ALoad(env(v)))
      case S.If(e1, e2, e3) =>
        val label_false = generateLabel("iffalse")
        val label_if_end = generateLabel("ifend")
        compile_expr(e1, env) ++ List(
          T.Unbox,
          T.If(BinOp.toCmp(BinOp.Eq), label_false)
        ) ++ compile_expr(e2, env) ++
          List(
            T.Goto(label_if_end),
            T.Labelize(label_false)
          ) ++ compile_expr(e3, env) ++
          List(T.Labelize(label_if_end))

      case S.Op(o, e1, e2) =>
        if (isArith(o)) {
          compile_expr(e1, env) ++ List(T.Unbox) ++ compile_expr(e2, env) ++
            List(
              T.Unbox,
              T.IOp(BinOp.toArith(o)),
              T.Box
            )
        } else {
          val label_true = generateLabel("booleantrue")
          val label_end = generateLabel("booleanend")
          compile_expr(e1, env) ++ List(T.Unbox) ++
            compile_expr(e2, env) ++ List(
              T.Unbox,
              T.Ificmp(BinOp.toCmp(o), label_true),
              T.Push(0),
              T.Goto(label_end),
              T.Labelize(label_true),
              T.Push(1),
              T.Labelize(label_end),
              T.Box
            )
        }

      /* Un exemple de primitif : le print_int */
      case S.Prim(Printint, List(e1)) =>
        compile_expr(e1, env) ++ List(T.Unbox, T.IPrint, T.Push(0), T.Box)
      /* Push(0) correspond au résultat de type unit du print_int */
      case _ => List() // TODO : traiter tous les cas manquants !
    }
  }

}
