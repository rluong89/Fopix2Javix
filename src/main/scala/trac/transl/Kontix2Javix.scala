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

  // Pour le tableswitch
  var return_labels = List[String]()
  var fun_index = 1000

  // Fonction pour modifier la variable count
  def setCount(x: Int): Unit = {
    count = x
  }

  def computeStackUse(
      p: List[T.Instruction],
      currentStackUse: Int,
      maxStackUse: Int
  ): Int = {
    p match {
      case Nil    => maxStackUse
      case h :: t =>
        // A remanier pour les remises a 0
        val stackInfo = javix.AST.stackUse(h)
        if (stackInfo.needed > currentStackUse) {
          throw new Invalid("Problème de stack")
        }
        val maxStack = Math.max(currentStackUse + stackInfo.max, maxStackUse)
        computeStackUse(t, currentStackUse + stackInfo.delta, maxStackUse)
    }
  }

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

  def generateLabel(s: String): String = {
    s + "_" + UUID.randomUUID().toString
  }

  def compile(progname: String, p: S.Program): T.Program = {
    val varsize = 100
    val stacksize = 10000
    val env: Env = Map.empty
    val (definitions, tailexpr) = (p.defs, p.main)
    //println(tailexpr)
    val (labelIndirectCall, funEnv) =
      generateFunEnv(Map.empty, List[String](), definitions, 1000)
    //println(fun_index)
    //println(definitions)

    val initKont = List(T.Push(funEnv("__RET")), T.Box, T.AStore(0))
    val initEnv = List(T.Push(0), T.ANewarray, T.AStore(1))
    val retKont = List(T.Labelize("__RET"), T.Return)
    val compiledDefs =
      compile_definitions(definitions, env, funEnv, labelIndirectCall)
    // println("INIT KONTENV :" + (initKont ++ initEnv))
    val mainInstrs =
      initKont ++ initEnv ++ compile_tail_expr(
        tailexpr,
        funEnv,
        env
      ) ++ retKont ++ compiledDefs
    val instrs = mainInstrs ++ List(
      T.Return,
      T.Labelize("dispatch"),
      T.Tableswitch(1000, labelIndirectCall, oupsLabel),
      T.Labelize(oupsLabel)
    )
    // println(compiledDefs)
    T.Program(progname, instrs, varsize, stacksize)
  }

  def compile_definitions(
      p: List[S.Definition],
      env: Env,
      funEnv: FunEnv,
      labelList: List[String]
      /*Definitions         Main            */
  ): (List[T.Instruction]) = {
    p match {
      case Nil => List()
      /* Richard */
      case S.DefCont(f, formals_env, r, e) :: tl =>
        val (store_instructions, extended_env) =
          store_env_args(formals_env, env)
        val extended_env_with_r = extended_env + (r -> 2)
        // println("OUIPDPAJZDPOAJZDJAZO")
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
      /* Yassine */
      case S.DefFun(f, args, e) :: tl =>
        // Arguments a partir de 2
        val (env_fun, _) = args.foldLeft((env, 2)) { (acc, elt) =>
          (acc._1 + (elt -> acc._2), acc._2 + 1)
        }
        val old_count = count
        setCount(args.size + 2)
        if (f.equals("map")) {
          println("ENV MAP : " + env_fun)
        }
        val function_instrs =
          List(T.Labelize(f)) ++ compile_tail_expr(e, funEnv, env_fun)
        // Plus de retour dispatch, swap
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

  def get_val_from_env(i: Int): List[T.Instruction] = {
    List(T.ALoad(1), T.Push(i), T.AALoad)
  }

  def args_storing(
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
            T.AStore(acc._2) :: (List(T.Comment("ENV : " + env)) ++ acc._3)
          )
      }
    compile_instrs ++ compile_store
  }

  def compile_tail_expr(
      be: S.TailExpr,
      funEnv: FunEnv,
      env: Env
  ): List[T.Instruction] = {
    val res = be match {

      /* Yassine */
      case S.Let(id, e1, e2) =>
        //print("LET" + count)
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

      /* Richard */
      case S.If((o, be1, be2), te1, te2) =>
        val label_else = generateLabel("label_else")
        println("GROS IF")
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

      /* Richard => Direct Yassine => Indirect */
      // Doit mettre en place le call indirect pour tester
      case S.Call(e, args) =>
        val be = compile_basic_expr(
          e,
          funEnv,
          env
        )
        be ++ args_storing(args, funEnv, env) ++ List(
          T.Unbox,
          T.Goto("dispatch")
        )
      /* Yassine Manipulations de tableaux faire sortir les env et les kont */
      case S.Ret(e) =>
        val compiled_basic = compile_basic_expr(e, funEnv, env)
        List(T.ALoad(0), T.Unbox) ++ compiled_basic ++ List(
          T.AStore(2),
          T.Comment("YO"),
          T.Goto("dispatch")
        )

      /* Richard Creations de tableaux */
      case S.PushCont(c, saves, e) =>
        val size = saves.length + 2
        val inter = compile_tail_expr(
          e,
          funEnv,
          env
        )
        //println("INTER : " + inter)
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
    //  println("RES : " + res)
    //println("EXPR : " + e)
    res
  }

  def compile_basic_expr(
      e: S.BasicExpr,
      funEnv: FunEnv,
      env: Env
  ): List[T.Instruction] = {
    val res = e match {
      /* Yassine */
      case S.Num(n) => List(T.Push(n), T.Box)
      case S.Str(s) => List(T.Ldc(s))
      case S.Fun(fid) =>
        val fun_index = funEnv(fid)
        List(T.Push(fun_index), T.Box)
      //List(T.Goto(fid))
      case S.Var(id) => List(T.ALoad(env(id)))

      /* Yassine */
      case S.BLet(id, e1, e2) =>
        if (id.equals("_")) {
          val old_count = count
          val res = compile_basic_expr(e1, funEnv, env) ++ List(T.Pop) ++
            compile_basic_expr(e2, funEnv, env)
          setCount(old_count)
          res
        } else {
          val current_count = count
          // println(count)
          val new_env = (env + (id -> current_count))
          setCount(count + 1)
          val r = compile_basic_expr(e1, funEnv, env) ++ List(
            T.AStore(current_count)
          ) ++
            compile_basic_expr(e2, funEnv, new_env)
          r
        }

      /* Richard */
      case S.BIf((o, be1, be2), e1, e2) =>
        print("go bif")
        val label_else = generateLabel("label_else")
        val label_end = generateLabel("end")
        compile_basic_expr(be1, funEnv, env) ++ List(T.Unbox) ++
        compile_basic_expr(be2, funEnv, env) ++ List(T.Unbox, T.Ificmp(neg(o), label_else)) ++
        compile_basic_expr(e1, funEnv, env) ++
        List(T.Goto(label_end),T.Labelize(label_else)) ++
        compile_basic_expr(e2, funEnv, env) ++ 
        List(T.Labelize(label_end))

      /*
        println(e)
        val label_true = generateLabel("booleantrue")
        val label_end = generateLabel("booleanend")
        val compile_cond =
          compile_basic_expr(be1, funEnv, env) ++ List(T.Unbox) ++
            compile_basic_expr(be2, funEnv, env) ++ List(
              T.Ificmp(o, label_true),
              T.Push(0),
              T.Goto(label_end),
              T.Labelize(label_true),
              T.Push(1),
              T.Labelize(label_end),
              T.Box
            )
        val label_false = generateLabel("iffalse")
        val label_if_end = generateLabel("ifend")
        compile_cond ++ List(
          T.Unbox,
          T.If(BinOp.toCmp(BinOp.Eq), label_false)
        ) ++ compile_basic_expr(e1, funEnv, env) ++
          List(
            T.Goto(label_if_end),
            T.Labelize(label_false)
          ) ++ compile_basic_expr(e2, funEnv, env) ++
          List(T.Labelize(label_if_end))
          */

      
      /* Richard */
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
      /* Yassine */
      case S.Prim(p, args) => handlePrim(p, args, funEnv, env)
    }
    res

  }

  //we suppose that we have the array on top of the stack
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
