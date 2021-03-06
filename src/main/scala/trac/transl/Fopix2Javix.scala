/* This module implements a compiler from Fopix to Javix. */
package trac.transl

import java.util.UUID
import trac.fopix.AST
import java.awt.Label

object Fopix2Javix {
  import trac._
  import trac.PrimOp._
  import trac.BinOp._
  import trac.fopix.{AST => S}
  import trac.javix.{AST => T}
  // Label oups
  val oupsLabel = "oups"

  // Environnement des variables
  type Env = Map[S.Ident, Int]

  /* Environnement des fonctions fid -> index de fonction */
  type FunEnv = Map[S.FunIdent, Int]

  // Permet de calculer le nombre de variables nécessaires
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

  // Permet de générer l'environnement des fonctions + la liste des labels des fonctions
  def generateFunEnv(
      funEnv: FunEnv,
      list_label: List[String],
      p: S.Program,
      funcount: Int
  ): (List[String], FunEnv) = {
    p match {
      case Nil =>
        (list_label, funEnv)
      case S.Def(fid, _, _) :: p =>
        return_index += 1
        generateFunEnv(
          (funEnv + (fid -> funcount)),
          list_label ++ List(fid),
          p,
          funcount + 1
        )
      case _ :: p => generateFunEnv(funEnv, list_label, p, funcount)
    }
  }
  // Passe direct Fopix2Javix
  def compile(progname: String, p: S.Program): T.Program = {
    // Taille de pile incalculable
    val stacksize = 10000
    val env: Env = Map.empty
    val (labelIndirectCall, funEnv) =
      generateFunEnv(Map.empty, List[String](), p, 1000)
    val instrs =
      compile_definitions(
        p,
        env,
        funEnv,
        labelIndirectCall,
        List[T.Instruction](),
        List[T.Instruction]()
      )
    val varsize = computeVarSize(instrs)
    T.Program(progname, instrs, varsize, stacksize)
  }
  // Variable globale qui permet de générer l'environnement
  var count = 0

  // Liste des label de retours + call indirects
  var return_labels = List[String]()
  // Variable pour générer les index de retour et call indirects
  var return_index = 1000

  // Fonction pour modifier count
  def setCount(x: Int): Unit = {
    count = x
  }

  // Compile les definitions
  def compile_definitions(
      p: List[S.Definition],
      env: Env,
      funEnv: FunEnv,
      labelList: List[String],
      main_space: List[T.Instruction],
      functions_space: List[T.Instruction]
  ): List[T.Instruction] = {
    p match {
      case Nil =>
        val program_instructions =
          main_space ++ List(T.Return) ++ functions_space
        /* Cas final on renvoie les accumulateurs main_space et functions_space
            avec le dispatch s'il y a des appels de fonctions */
        if (return_labels.length == 0) {
          program_instructions
        } else {
          program_instructions ++ List(
            T.Labelize("dispatch"),
            T.Tableswitch(1000, labelList ++ return_labels, oupsLabel)
          ) ++
            List(T.Labelize(oupsLabel))
        }
      case S.Val(x, e) :: p =>
        // On calcule e et on étend l'environnement si nécessaire
        val instructions = compile_expr(e, funEnv, env)
        val optionX = env get (x)
        val (newInstruction, new_env) =
          optionX match {
            case Some(value) => (List(T.AStore(value)), env)
            case None =>
              if (x.equals("_")) {
                (List(T.Pop), env)
              } else {
                val current_count = count
                setCount(count + 1)
                (List(T.AStore(current_count)), (env + (x -> current_count)))
              }
          }
        val main_instrs = instructions ++ newInstruction
        compile_definitions(
          p,
          new_env,
          funEnv,
          labelList,
          main_space ++ main_instrs,
          functions_space
        )
      case S.Def(fid, args, e) :: p =>
        val (env_fun, _) = args.foldLeft((env, 0)) { (acc, elt) =>
          (acc._1 + (elt -> acc._2), acc._2 + 1)
        }
        val old_count = count
        count = args.size
        val function_instrs =
          List(T.Labelize(fid)) ++ compile_expr(e, funEnv, env_fun) ++
            List(T.Swap, T.Goto("dispatch"))
        count = old_count
        compile_definitions(
          p,
          env,
          funEnv,
          labelList,
          main_space,
          functions_space ++ function_instrs
        )
    }
  }

  // Fonction pour générer des Labels uniques
  def generateLabel(s: String): String = {
    s + "_" + UUID.randomUUID().toString
  }

  // Fonction d'archivage des arguments
  def archiving(
      args: List[S.Expr],
      env: Env
  ): (List[T.Instruction], List[Integer]) = {
    val (instructions, _, archiving_indexes) =
      args.foldLeft((List[T.Instruction](), 0, List[Integer]())) { (acc, elt) =>
        if (env.values.exists(_ == acc._2)) {
          (acc._1 ++ List(T.ALoad(acc._2)), acc._2 + 1, acc._2 :: acc._3)
        } else {
          (acc._1, acc._2 + 1, acc._3)
        }
      }
    (instructions, archiving_indexes)
  }

  // Restore les variables de la JVM après l'appel d'une fonction
  def restoration(archiving_indexes: List[Integer]): List[T.Instruction] = {
    archiving_indexes.foldLeft(List[T.Instruction]()) { (acc, elt) =>
      acc ++ List(T.Swap, T.AStore(elt))
    }
  }

  // Calcul et stockage des arguments pour le Call
  def args_compiling_storing(
      args: List[S.Expr],
      funEnv: FunEnv,
      env: Env
  ): List[T.Instruction] = {
    val (compile_instrs, _, compile_store) =
      args.foldLeft((List[T.Instruction](), 0, List[T.Instruction]())) {
        (acc, elt) =>
          (
            acc._1 ++ compile_expr(elt, funEnv, env),
            acc._2 + 1,
            T.AStore(acc._2) :: acc._3
          )
      }
    compile_instrs ++ compile_store
  }

  // Compile une Expr
  def compile_expr(e: S.Expr, funEnv: FunEnv, env: Env): List[T.Instruction] = {
    e match {
      case S.Num(n) => List(T.Push(n), T.Box)
      case S.Str(s) => List(T.Ldc(s))
      case S.Var(v) => List(T.ALoad(env(v)))
      case S.If(e1, e2, e3) =>
        val label_false = generateLabel("iffalse")
        val label_if_end = generateLabel("ifend")
        compile_expr(e1, funEnv, env) ++ List(
          T.Unbox,
          T.If(BinOp.toCmp(BinOp.Eq), label_false)
        ) ++ compile_expr(e2, funEnv, env) ++
          List(
            T.Goto(label_if_end),
            T.Labelize(label_false)
          ) ++ compile_expr(e3, funEnv, env) ++
          List(T.Labelize(label_if_end))
      case S.Op(o, e1, e2) =>
        if (isArith(o)) {
          compile_expr(e1, funEnv, env) ++ List(T.Unbox) ++ compile_expr(
            e2,
            funEnv,
            env
          ) ++
            List(
              T.Unbox,
              T.IOp(BinOp.toArith(o)),
              T.Box
            )
        } else {
          val label_true = generateLabel("booleantrue")
          val label_end = generateLabel("booleanend")
          compile_expr(e1, funEnv, env) ++ List(T.Unbox) ++
            compile_expr(e2, funEnv, env) ++ List(
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
        if (id.equals("_")) {
          val old_count = count
          val res = compile_expr(e1, funEnv, env) ++ List(T.Pop) ++
            compile_expr(e2, funEnv, env)
          count = old_count
          res
        } else {
          val current_count = count
          val new_env = (env + (id -> current_count))
          setCount(count + 1)
          compile_expr(e1, funEnv, env) ++ List(T.AStore(current_count)) ++
            compile_expr(e2, funEnv, new_env)
        }
      case S.Fun(fid) =>
        val fun_index = funEnv(fid)
        List(T.Push(fun_index), T.Box)
      case S.Call(f, args) =>
        val goto_fid = compile_expr(f, funEnv, env)
        val current_return_index = return_index
        val return_label = "return" + return_index.toString
        val (archiving_instrs, archiving_indexes) = archiving(args, env)
        return_index += 1
        return_labels ++= List(return_label)
        /* 1 Archivage des vars */
        val res = archiving_instrs ++
          /* 2 Stocke code de label de retour */
          List(T.Push(current_return_index)) ++
          /* 3 compile E + unbox */
          goto_fid ++ List(T.Unbox) ++
          /* 4 5 compilation et stockage des arguments */
          args_compiling_storing(args, funEnv, env) ++
          List(T.Goto("dispatch")) ++
          List(T.Labelize(return_label)) ++
          restoration(archiving_indexes)
        res
      case S.Prim(prim, list) =>
        (prim, list) match {
          case (New, List(e1)) =>
            compile_expr(e1, funEnv, env) ++ List(T.Unbox, T.ANewarray)
          case (Get, List(e1, e2)) =>
            compile_expr(e1, funEnv, env) ++ List(T.Checkarray) ++
              compile_expr(e2, funEnv, env) ++ List(T.Unbox, T.AALoad)
          case (Set, List(e1, e2, e3)) =>
            compile_expr(e1, funEnv, env) ++ compile_expr(
              e2,
              funEnv,
              env
            ) ++ List(T.Unbox) ++
              compile_expr(e3, funEnv, env) ++ List(T.AAStore, T.Push(0), T.Box)
          case (Tuple, _) =>
            val count = list.length
            val l = list.foldLeft((List[T.Instruction](), 0)) { (acc, elt) =>
              (
                acc._1 ++ List(T.Dup, T.Push(acc._2)) ++ (compile_expr(
                  elt,
                  funEnv,
                  env
                )) ++ List(T.AAStore),
                acc._2 + 1
              )
            }
            List(T.Push(count), T.ANewarray) ++ l._1
          case (Printint, List(e1)) =>
            compile_expr(e1, funEnv, env) ++ List(
              T.Unbox,
              T.IPrint,
              T.Push(0),
              T.Box
            )
          case (Printstr, List(e1)) =>
            compile_expr(e1, funEnv, env) ++ List(T.SPrint, T.Push(0), T.Box)
          case (Cat, List(e1, e2)) =>
            compile_expr(e1, funEnv, env) ++ compile_expr(
              e2,
              funEnv,
              env
            ) ++ List(T.SCat)
        }
    }
  }
}
