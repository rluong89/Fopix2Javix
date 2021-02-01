/* This module implements a compiler from Fopix to Javix. */

// TODO : To finish !!

package trac.transl

import java.util.UUID

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

  def compile(progname: String, p: S.Program): T.Program = {
    val varsize = 100
    val stacksize = 10000
    //val instrs = List(T.Comment("Todo!!"),T.Return)
    val env: Env = Map.empty
    val instrs = compile_definitions(p, env) ++ List(T.Return)
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
            case Some(value) => (T.IStore(value), env)
            case None =>
              count += 1
              (T.IStore(count), (env + (x -> count)))
          }
        instructions ++ List(newInstruction) ++
          (compile_definitions(p, new_env))

      case S.Def(_, _, _) :: p => compile_definitions(p, env)
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
      case S.Num(n)        => List(T.Push(n))
      case S.Str(s)        => List(T.Ldc(s))
      case S.Var(v)        => List(T.ILoad(env(v)))
      case S.Op(o, e1, e2) =>
        /* code si o est un opérateur arithmetique
         * TODO: que faire si o est une comparaison ? */
        if (isArith(o)) {
          compile_expr(e1, env) ++ compile_expr(e2, env) ++ List(
            T.IOp(BinOp.toArith(o))
          )
        } else {
          val label_true = generateLabel("booleantrue")
          val label_end = generateLabel("booleanend")
          compile_expr(e1, env) ++ compile_expr(e2, env) ++ List(
            T.Ificmp(BinOp.toCmp(o), label_true),
            T.Push(0),
            T.Goto(label_end),
            T.Labelize(label_true),
            T.Push(1),
            T.Labelize(label_end)
          )
        }

      /* Un exemple de primitif : le print_int */
      case S.Prim(Printint, List(e1)) =>
        compile_expr(e1, env) ++ List(T.IPrint, T.Push(0))
      /* Push(0) correspond au résultat de type unit du print_int */
      case _ => List() // TODO : traiter tous les cas manquants !
    }
  }

}
