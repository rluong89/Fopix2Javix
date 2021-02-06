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
            case Some(value) => (T.AStore(value), env)
            case None =>
              count += 1
              (T.AStore(count), (env + (x -> count)))
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

  def generateDup(n : Integer): List[T.Instruction] = {
    def aux(n : Integer, acc : List[T.Instruction]) : List[T.Instruction] = {
      if (n == 0) 
        acc
      else
        aux(n - 1, T.Dup :: acc)
    }
    aux(n, List());
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
      case S.Let(id, e1, e2) => 
        val current_count = count
        val new_env = (env + (id -> current_count))
        count += 1
        compile_expr(e1, env) ++ List(T.AStore(current_count)) ++ 
        compile_expr(e2, new_env)

          /*RICHARD*/
      case S.Fun(fid) => 
      List()
          /*RICHARD DIRECT*/
          /*YASSINE INDIRECT*/
      /* Un exemple de primitif : le print_int */
      case S.Prim(prim, list) =>
        (prim, list) match {
          case (New, List(e1)) =>
            compile_expr(e1, env) ++ List(T.Unbox, T.ANewarray)
          case (Get, List(e1, e2)) =>
            compile_expr(e1, env) ++ List(T.Checkarray) ++ compile_expr(e2, env) ++ List(T.Unbox, T.AALoad)
          case (Set, List(e1, e2, e3)) =>
            compile_expr(e1, env) ++ compile_expr(e2, env) ++ List(T.Unbox) ++ 
            compile_expr(e3, env) ++ List(T.AAStore, T.Push(0), T.Box)
          case (Tuple, _) =>
            val count = list.length
            val l = list.foldLeft((List[T.Instruction](), 0)) {
              (acc, elt) => 
                (acc._1 ++ List(T.Push(acc._2)) ++ (compile_expr(elt, env))
                ++ List(T.AAStore), acc._2 + 1)
            }
            List(T.Push(count), T.ANewarray) ++ generateDup(count) ++ l._1
          case (Printint, List(e1)) =>
            compile_expr(e1, env) ++ List(T.Unbox, T.IPrint, T.Push(0), T.Box)
          case (Printstr, List(e1)) =>
            compile_expr(e1, env) ++ List(T.SPrint, T.Push(0), T.Box)
          case (Cat, List(e1, e2)) =>
            compile_expr(e1, env) ++  compile_expr(e2, env) ++ List(T.SCat)   
        }

      
      /* Push(0) correspond au résultat de type unit du print_int */
      case _ => List() // je fé les prims mon reuf TODO : traiter tous les cas manquants !
    }
  }

}
