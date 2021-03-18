/* This module implements a compiler from Kontix to Javix. */

// TODO : To finish !!

package trac.transl
import trac.kontix.AST
import java.util.UUID
import scala.collection.immutable
import _root_.trac.javix.AST

object Kontix2Javix {

  import trac._

  import trac.PrimOp._
  import trac.BinOp._
  import trac.CompOp._
  import trac.kontix.{AST => S}
  import trac.javix.{AST => T}

  final case class CustomException(
      private val message: String = "",
      private val cause: Throwable = None.orNull
  ) extends Exception(message, cause)

  val oupsLabel = "oups"

  type Env = Map[S.Ident, Int]

  /* maps fun/kont id to their tablewitch index */
  type FunEnv = Map[String, Int]

  // Pour les var JVM
  var count = 2

  // Compteur pour gérer les index de fonctions
  var fun_index = 1000

  // Fonction pour modifier la variable count
  def setCount(x: Int): Unit = {
    count = x
  }

  // Calcul de la taille de la pile
  def computeStackUse(
      p: List[T.Instruction],
      currentStackUse: Int,
      maxStackUse: Int
  ): Int = {
    p match {
      case Nil    => maxStackUse
      case h :: t =>
        // A remanier pour les remises à 0
        val stackInfo = javix.AST.stackUse(h)
        val maxStack = Math.max(currentStackUse + stackInfo.max, maxStackUse)

        val newCurrentStack = h match {
          case T.Goto("dispatch") => currentStackUse
          case T.Goto(s) =>
            if (s.startsWith("endbif")) {
              currentStackUse - 1
            } else { 0 }
          case T.Labelize(s) =>
            if (s.startsWith("label_bifelse") || s.startsWith("endbif")) {
              currentStackUse
            } else { 0 }
          case _ =>
            currentStackUse + stackInfo.delta
        }
        computeStackUse(t, newCurrentStack, maxStack)
    }
  }

  /* Génère l'environnement des fonctions, continuations
     et la liste des label des fonctions, continuations */
  def generateFunEnv(
      funEnv: FunEnv,
      list_label: List[String],
      p: List[S.Definition],
      funcount: Int
  ): (List[String], FunEnv) = {
    p match {
      case Nil =>
        fun_index += 1
        (list_label ++ List("__RET"), (funEnv + ("__RET" -> funcount)))
      case S.DefFun(fid, _, _) :: p =>
        // Index du tableswitch return + calls
        fun_index += 1
        generateFunEnv(
          (funEnv + (fid -> funcount)),
          list_label ++ List(fid),
          p,
          funcount + 1
        )
      case S.DefCont(fid, _, _, _) :: p =>
        fun_index += 1
        generateFunEnv(
          (funEnv + (fid -> funcount)),
          list_label ++ List(fid),
          p,
          funcount + 1
        )
    }
  }

  // Calcul de nombre de variables
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

  // Génére un Label unique
  def generateLabel(s: String): String = {
    s + "_" + UUID.randomUUID().toString
  }

  // Passe Kontix à Javix
  def compile(progname: String, p: S.Program): T.Program = {
    val env: Env = Map.empty
    val (definitions, tailexpr) = (p.defs, p.main)
    val (labelIndirectCall, funEnv) =
      generateFunEnv(Map.empty, List[String](), definitions, 1000)
    val initKont = List(T.Push(funEnv("__RET")), T.Box, T.AStore(0))
    val initEnv = List(T.Push(0), T.ANewarray, T.AStore(1))
    val retKont = List(T.Labelize("__RET"), T.Return)
    val compiledDefs =
      compile_definitions(definitions, env, funEnv, labelIndirectCall)
    // Instruction du main
    val mainInstrs =
      initKont ++ initEnv ++ compile_tail_expr(
        tailexpr,
        funEnv,
        env
      ) ++ retKont ++ compiledDefs
    // Code du programme
    val instrs = mainInstrs ++ List(
      T.Return,
      T.Labelize("dispatch"),
      T.Tableswitch(1000, labelIndirectCall, oupsLabel),
      T.Labelize(oupsLabel)
    )
    // Calcul variables et taille de pile
    val varsize = computeVarSize(instrs)
    val stacksize = computeStackUse(instrs, 0, 0)
    T.Program(progname, instrs, varsize, stacksize)
  }

  // Compilation des définitions
  def compile_definitions(
      p: List[S.Definition],
      env: Env,
      funEnv: FunEnv,
      labelList: List[String]
  ): (List[T.Instruction]) = {
    p match {
      case Nil => List()
      case S.DefCont(f, formals_env, r, e) :: tl =>
        val (store_instructions, extended_env) =
          store_env_args(formals_env, env)
        val extended_env_with_r = extended_env + (r -> 2)
        val currentRes =
          List(T.Labelize(f)) ++ get_val_from_env(0) ++ List(T.AStore(0)) ++
            store_instructions ++ get_val_from_env(1) ++
            List(T.AStore(1)) ++ compile_tail_expr(
              e,
              funEnv,
              extended_env_with_r
            )
        val recursive_res = compile_definitions(
          tl,
          env,
          funEnv,
          labelList
        )
        currentRes ++ recursive_res
      case S.DefFun(f, args, e) :: tl =>
        // Arguments à partir de 2
        val (env_fun, _) = args.foldLeft((env, 2)) { (acc, elt) =>
          (acc._1 + (elt -> acc._2), acc._2 + 1)
        }
        val old_count = count
        setCount(args.size + 2)
        val function_instrs =
          List(T.Labelize(f)) ++ compile_tail_expr(e, funEnv, env_fun)
        setCount(old_count)
        val recursive_res = compile_definitions(
          tl,
          env,
          funEnv,
          labelList
        )
        function_instrs ++ recursive_res
    }
  }

  // Compilation des tailExpr
  def compile_tail_expr(
      e: S.TailExpr,
      funEnv: FunEnv,
      env: Env
  ): List[T.Instruction] = {
    e match {
      case S.Let(id, e1, e2) =>
        if (id.equals("_")) {
          val old_count = count
          val res = compile_basic_expr(e1, funEnv, env) ++ List(T.Pop) ++
            compile_tail_expr(e2, funEnv, env)
          setCount(old_count)
          res
        } else {
          val current_count = count
          val new_env = (env + (id -> current_count))
          setCount(count + 1)
          compile_basic_expr(e1, funEnv, env) ++ List(
            T.AStore(current_count)
          ) ++
            compile_tail_expr(e2, funEnv, new_env)
        }

      case S.If((o, be1, be2), te1, te2) =>
        val label_else = generateLabel("label_else")
        compile_basic_expr(be1, funEnv, env) ++ List(
          T.Unbox
        ) ++ compile_basic_expr(
          be2,
          funEnv,
          env
        ) ++ List(T.Unbox) ++
          List(T.Ificmp(neg(o), label_else)) ++
          compile_tail_expr(te1, funEnv, env) ++
          List(T.Labelize(label_else)) ++
          compile_tail_expr(te2, funEnv, env)

      case S.Call(e, args) =>
        val be = compile_basic_expr(
          e,
          funEnv,
          env
        )
        be ++ args_compiling_storing(args, funEnv, env) ++ List(
          T.Unbox,
          T.Goto("dispatch")
        )
      case S.Ret(e) =>
        val compiled_basic = compile_basic_expr(e, funEnv, env)
        List(T.ALoad(0), T.Unbox) ++ compiled_basic ++ List(
          T.AStore(2),
          T.Goto("dispatch")
        )
      case S.PushCont(c, saves, e) =>
        val size = saves.length + 2
        val inter = compile_tail_expr(
          e,
          funEnv,
          env
        )
        List(
          T.Push(size),
          T.ANewarray,
          T.Dup,
          T.Push(0),
          T.ALoad(0),
          T.AAStore,
          T.Dup,
          T.Push(1),
          T.ALoad(1),
          T.AAStore
        ) ++
          fill_array_from(2, saves, funEnv, env) ++ List(T.AStore(1)) ++
          List(T.Push(funEnv(c)), T.Box, T.AStore(0)) ++ inter
    }
  }

  // Compilation des basicExpr
  def compile_basic_expr(
      e: S.BasicExpr,
      funEnv: FunEnv,
      env: Env
  ): List[T.Instruction] = {
    e match {
      case S.Num(n) => List(T.Push(n), T.Box)
      case S.Str(s) => List(T.Ldc(s))
      case S.Fun(fid) =>
        val fun_index = funEnv(fid)
        List(T.Push(fun_index), T.Box)
      case S.Var(id) => List(T.ALoad(env(id)))
      case S.BLet(id, e1, e2) =>
        if (id.equals("_")) {
          val old_count = count
          val res = compile_basic_expr(e1, funEnv, env) ++ List(T.Pop) ++
            compile_basic_expr(e2, funEnv, env)
          setCount(old_count)
          res
        } else {
          val current_count = count
          val new_env = (env + (id -> current_count))
          setCount(count + 1)
          val r = compile_basic_expr(e1, funEnv, env) ++ List(
            T.AStore(current_count)
          ) ++
            compile_basic_expr(e2, funEnv, new_env)
          r
        }
      case S.BIf((o, be1, be2), e1, e2) =>
        val label_else = generateLabel("label_bifelse")
        val label_end = generateLabel("endbif")
        compile_basic_expr(be1, funEnv, env) ++ List(T.Unbox) ++
          compile_basic_expr(be2, funEnv, env) ++ List(
            T.Unbox,
            T.Ificmp(neg(o), label_else)
          ) ++
          compile_basic_expr(e1, funEnv, env) ++
          List(T.Goto(label_end), T.Labelize(label_else)) ++
          compile_basic_expr(e2, funEnv, env) ++
          List(T.Labelize(label_end))

      case S.Op(o, e1, e2) =>
        compile_basic_expr(e1, funEnv, env) ++ List(
          T.Unbox
        ) ++ compile_basic_expr(
          e2,
          funEnv,
          env
        ) ++
          List(
            T.Unbox,
            T.IOp(o),
            T.Box
          )
      case S.Prim(p, args) => handlePrim(p, args, funEnv, env)
    }
  }

  /*Stockage des arguments de l'environnement dans les variables de la JVM et
    extension de l'environnement */
  def store_env_args(
      args: List[S.Ident],
      env: Env
  ): (List[T.Instruction], Env) = {
    val (instructions, _, extended_env) =
      args.foldLeft((List[T.Instruction](), 2, env)) { (acc, elt) =>
        (
          acc._1 ++ get_val_from_env(acc._2) ++
            List(T.AStore(acc._2 + 1)),
          acc._2 + 1,
          acc._3 + (elt -> (acc._2 + 1))
        )
      }
    (instructions, extended_env)
  }

  // Met le contenu de l'environnement à la case i sur le haut de la pile
  def get_val_from_env(i: Int): List[T.Instruction] = {
    List(T.ALoad(1), T.Push(i), T.AALoad)
  }

  // Calcul et stockage des arguments pour le Call
  def args_compiling_storing(
      args: List[S.BasicExpr],
      funEnv: FunEnv,
      env: Env
  ): List[T.Instruction] = {
    val (compile_instrs, _, compile_store) =
      args.foldLeft((List[T.Instruction](), 2, List[T.Instruction]())) {
        (acc, elt) =>
          (
            acc._1 ++ compile_basic_expr(elt, funEnv, env),
            acc._2 + 1,
            T.AStore(acc._2) :: acc._3
          )
      }
    compile_instrs ++ compile_store
  }

  /* Remplit un tableau depuis la case i à partir d'une liste de String ou de BasicExpr, 
     on suppose l'addrese du tableau au sommet de la pile */

  def fill_array_from(
      i: Integer,
      l: List[Any],
      funEnv: FunEnv,
      env: Env
  ): List[T.Instruction] = {
    val count = l.length
    val (instructions, _) = l.foldLeft((List[T.Instruction](), i)) {
      (acc, elt) =>
        val compiled_elt =
          elt match {
            case elt: String      => List(T.ALoad(env(elt)))
            case elt: S.BasicExpr => compile_basic_expr(elt, funEnv, env)
            case _                => throw CustomException("Not handled")
          }
        (
          acc._1 ++ List(T.Dup, T.Push(acc._2)) ++ compiled_elt ++ List(
            T.AAStore
          ),
          acc._2 + 1
        )
    }
    instructions
  }

  // Gère le cas des primitives
  def handlePrim(
      p: PrimOp.T,
      args: List[S.BasicExpr],
      funEnv: FunEnv,
      env: Env
  ): List[T.Instruction] = {
    (p, args) match {
      case (New, List(e1)) =>
        compile_basic_expr(e1, funEnv, env) ++ List(T.Unbox, T.ANewarray)
      case (Get, List(e1, e2)) =>
        compile_basic_expr(e1, funEnv, env) ++ List(T.Checkarray) ++
          compile_basic_expr(e2, funEnv, env) ++ List(T.Unbox, T.AALoad)
      case (Set, List(e1, e2, e3)) =>
        compile_basic_expr(e1, funEnv, env) ++ compile_basic_expr(
          e2,
          funEnv,
          env
        ) ++ List(T.Unbox) ++
          compile_basic_expr(e3, funEnv, env) ++ List(
            T.AAStore,
            T.Push(0),
            T.Box
          )
      case (Tuple, list) =>
        List(T.Push(list.length), T.ANewarray) ++
          fill_array_from(0, list, funEnv, env)
      case (Printint, List(e1)) =>
        compile_basic_expr(e1, funEnv, env) ++ List(
          T.Unbox,
          T.IPrint,
          T.Push(0),
          T.Box
        )
      case (Printstr, List(e1)) =>
        compile_basic_expr(e1, funEnv, env) ++ List(T.SPrint, T.Push(0), T.Box)
      case (Cat, List(e1, e2)) =>
        compile_basic_expr(e1, funEnv, env) ++ compile_basic_expr(
          e2,
          funEnv,
          env
        ) ++ List(T.SCat)
    }
  }
}