/* This module implements a compiler from Kontix to Javix. */

// TODO : To finish !!

package trac.transl
import trac.kontix.AST
import java.util.UUID
import scala.collection.immutable

object Kontix2Javix {

  import trac._

  import trac.PrimOp._
  import trac.BinOp._
  import trac.kontix.{AST => S}
  import trac.javix.{AST => T}

  val oupsLabel = "oups"

  type Env = Map[S.Ident, Int]

  /* maps fun/kont id to their tablewitch index */
  type FunEnv = Map[String, Int]
  // Pour les var JVM
  var count = 0
  // Pour le tableswitch
  var return_labels = List[String]()
  var return_index = 1000

  // Fonction pour modifier la variable count
  def setCount(x: Int): Unit = {
    count = x
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
    val instrs = List(T.Comment("Todo!!"), T.Return)
    T.Program(progname, instrs, varsize, stacksize)
  }

  def compile_definitions(
      p: List[S.Definition],
      env: Env,
      funEnv: FunEnv,
      labelList: List[String]
      /*Definitions         Main            */
  ): (List[T.Instruction], List[T.Instruction]) = {
    p match {
      case Nil => (List(), List())
      /* Richard */
      case S.DefCont(f, env, arg, e) :: tl => (List(), List())
      /* Yassine */
      case S.DefFun(f, args, e) :: tl => (List(), List())
    }
  }

  def compile_tail_expr(
      e: S.TailExpr,
      funEnv: FunEnv,
      env: Env
  ): List[T.Instruction] = {
    e match {

      /* Yassine */
      case S.Let(id, e1, e2) => List()

      /* Richard */
      case S.If(c, e1, e2) => List()

      /* Richard => Direct Yassine => Indirect */
      case S.Call(e, args) => List()

      /* Yassine */
      case S.Ret(e) =>
        /*
      ALoad(0)
      Unbox
      Puis calcul de be
      AStore(2) ; resultat de be
      Goto dispatch
         */
        val compiled_basic = compile_basic_expr(e, funEnv, env)
        List(T.ALoad(0), T.Unbox, T.AStore(2)) ++ compiled_basic ++ List(
          T.Goto("dispatch")
        )

      /* Richard Creations de tableaux */
      case S.PushCont(c, saves, e) => List()
    }
  }

  def compile_basic_expr(
      e: S.BasicExpr,
      funEnv: FunEnv,
      env: Env
  ): List[T.Instruction] = {
    e match {
      /* Yassine */
      case S.Num(n) => List(T.Push(n), T.Box)
      case S.Str(s) => List(T.Ldc(s))
      case S.Fun(fid) =>
        val fun_index = funEnv(fid)
        List(T.Push(fun_index), T.Box)
      case S.Var(id) => List(T.ALoad(env(id)))

      /* Yassine */
      case S.BLet(id, e1, e2) => List()
      /* Richard */
      case S.BIf((o, be1, be2), e1, e2) =>
        val label_true = generateLabel("booleantrue")
        val label_end = generateLabel("booleanend")
        compile_basic_expr(be1, funEnv, env) ++ List(T.Unbox) ++
          compile_basic_expr(be2, funEnv, env) ++ List(
            T.Unbox,
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
        compile_basic_expr(e1, funEnv, env) ++ List(
          T.Unbox,
          T.If(BinOp.toCmp(BinOp.Eq), label_false)
        ) ++ compile_basic_expr(e1, funEnv, env) ++
          List(
            T.Goto(label_if_end),
            T.Labelize(label_false)
          ) ++ compile_basic_expr(e2, funEnv, env) ++
          List(T.Labelize(label_if_end))
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
        val count = list.length

        val l = list.foldLeft((List[T.Instruction](), 0)) { (acc, elt) =>
          (
            acc._1 ++ List(T.Push(acc._2)) ++ (compile_basic_expr(
              elt,
              funEnv,
              env
            )) ++ List(T.AAStore),
            acc._2 + 1
          )
        }
        List(T.Push(count), T.ANewarray) ++ generate_dup(count) ++ l._1
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
