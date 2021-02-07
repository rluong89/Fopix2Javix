/* This module implements a compiler from Fopix to Javix. */

// TODO : To finish !!

package trac.transl

import java.util.UUID
import trac.fopix.AST

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

  var return_labels = List()

  var return_index = 1000

  var archiving_list = List()

  def compile_definitions(
      p: List[S.Definition],
      env: Env
  ): List[T.Instruction] = {
    /* TODO: à completer, on ne s'occupe ici que du premier Val ! */
    p match {
      case Nil => /*
        val label_dispatch = generateLabel("dispatch")
        val label_oups = generateLabel("oups")
        List(
          T.Labelize(label_dispatch),
          T.Tableswitch(1000, return_labels, label_oups)
        )*/
        List()
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
      case S.Def(fid, args, e) :: p =>
        /* val label_fid = generateLabel(fid)
        val label_dispatch = generateLabel("dispatch")
        List(T.Labelize(label_fid)) ++ compile_expr(e, env) ++
          List(T.Swap, T.Goto(label_dispatch)) */
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

  def generate_dup(n: Integer): List[T.Instruction] = {
    def aux(n: Integer, acc: List[T.Instruction]): List[T.Instruction] = {
      if (n == 0)
        acc
      else
        aux(n - 1, T.Dup :: acc)
    }
    aux(n, List());
  }

  def generate_store(n: Integer): List[T.Instruction] = {
    def aux(i: Integer, acc: List[T.Instruction]): List[T.Instruction] = {
      if (i == n)
        T.AStore(i) :: acc
      else
        aux(i + 1, T.AStore(i) :: acc)
    }
    aux(0, List())
  }

  def archiving(args_len: Integer, env: Env): List[T.Instruction] = {
    archiving_list = List()
    def aux(i: Integer, acc: List[T.Instruction]): List[T.Instruction] = {
      if (i == args_len)
        return acc
      if (env.values.exists(_ == i)) {
        archiving_list ++ List(i)
        aux(i + 1, acc ++ List(T.ALoad(i)))
      } else {
        aux(i + 1, acc)
      }
    }
    aux(0, List())
  }

  def restoration(): List[T.Instruction] = {
    archiving_list.foldLeft(List[T.Instruction]()) { (acc, elt) =>
      acc ++ List(T.AStore(elt))
    }
  }

  def args_storing(args: List[S.Expr], env: Env): List[T.Instruction] = {
    val (compile_instrs, index) =
      args.foldLeft((List[T.Instruction](), 0)) { (acc, elt) =>
        (
          acc._1 ++
            compile_expr(elt, env),
          acc._2 + 1
        )
      }
    compile_instrs ++ generate_store(index)
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

      /*yassine*/
      case S.Fun(fid) =>
        List(T.Ldc(fid)) //archi faux mais provisoire pour recup le fid...
      case S.Call(f, args) =>
        val cp_fid = compile_expr(f, env)
        val fid = cp_fid.head match {
          case T.Ldc(fid) => fid
          case _          => throw new Exception
        }
        archiving(args.length, env)
        args_storing(args, env)
        val f_label = generateLabel(fid)
        val return_label = generateLabel("return" + return_index.toString)
        List(T.Push(return_index), T.Goto(f_label), T.Labelize(return_label))
        return_index += 1
        return_labels ++ List(return_label)
        restoration()
      /*RICHARD DIRECT*/
      /*YASSINE INDIRECT*/
      /* Un exemple de primitif : le print_int */
      case S.Prim(prim, list) =>
        (prim, list) match {
          case (New, List(e1)) =>
            compile_expr(e1, env) ++ List(T.Unbox, T.ANewarray)
          case (Get, List(e1, e2)) =>
            compile_expr(e1, env) ++ List(T.Checkarray) ++ compile_expr(
              e2,
              env
            ) ++ List(T.Unbox, T.AALoad)
          case (Set, List(e1, e2, e3)) =>
            compile_expr(e1, env) ++ compile_expr(e2, env) ++ List(T.Unbox) ++
              compile_expr(e3, env) ++ List(T.AAStore, T.Push(0), T.Box)
          case (Tuple, _) =>
            val count = list.length
            val l = list.foldLeft((List[T.Instruction](), 0)) { (acc, elt) =>
              (
                acc._1 ++ List(T.Push(acc._2)) ++ (compile_expr(elt, env))
                  ++ List(T.AAStore),
                acc._2 + 1
              )
            }
            List(T.Push(count), T.ANewarray) ++ generate_dup(count) ++ l._1
          case (Printint, List(e1)) =>
            compile_expr(e1, env) ++ List(T.Unbox, T.IPrint, T.Push(0), T.Box)
          case (Printstr, List(e1)) =>
            compile_expr(e1, env) ++ List(T.SPrint, T.Push(0), T.Box)
          case (Cat, List(e1, e2)) =>
            compile_expr(e1, env) ++ compile_expr(e2, env) ++ List(T.SCat)
        }

      /* Push(0) correspond au résultat de type unit du print_int */
      case _ =>
        List() // je fé les prims mon reuf TODO : traiter tous les cas manquants !
    }
  }

}
