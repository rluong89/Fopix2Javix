/* This module implements the compilation from Anfix to Kontix */

// TODO : To finish !!

package trac.transl
import scala.collection.immutable.Nil
import trac.anfix.AST.Val
import trac.anfix.AST.Def
import trac.anfix.AST.Simple
import trac.anfix.AST.Let
import trac.anfix.AST.If
import trac.anfix.AST.Op
import trac.anfix.AST.Prim
import trac.anfix.AST.Call
import trac.anfix.AST.Num
import trac.anfix.AST.Str
import trac.anfix.AST.Fun
import trac.anfix.AST.Var
import trac.kontix.AST
import trac.fopix._

object Anfix2Kontix {

  import trac._
  import trac.anfix.{AST => S}
  import trac.kontix.{AST => T}

  final case class CustomException(
      private val message: String = "",
      private val cause: Throwable = None.orNull
  ) extends Exception(message, cause)

  var global_kont: List[T.Definition] = List()

  // Vérifie si une Definition est une Val
  def isVal(e: S.Definition): Boolean = {
    e match {
      case Val(_, _)         => true
      case Def(fid, args, e) => false
    }
  }

  // Fonction qui permettera de compiler une suite de val en suite de let
  def mainToLetRecursive(p: S.Program): (S.Expr, List[S.Definition]) = {
    p match {
      case Nil => (S.Simple(S.Num(0)), List())
      case S.Val(id, e) :: tl =>
        tl match {
          case Nil => (e, List())
          case _ =>
            val res = mainToLetRecursive(tl)
            val let = S.Let(id, e, res._1)
            (let, res._2)
        }
      case e :: tl =>
        val res = mainToLetRecursive(tl)
        (res._1, e :: res._2)
    }
  }

  // Traduit un main Anfix en suite de Let
  def mainToLet(p: S.Program): S.Program = {
    val vals_definitions = p.filter(isVal)
    val def_definitions = p.filter(x => !isVal(x))
    val sortedProgram = def_definitions ++ vals_definitions
    val (lets, definitions) = mainToLetRecursive(sortedProgram)
    lets match {
      case S.Simple(S.Num(0)) => definitions
      case _                  => definitions :+ S.Val("_", lets)
    }
  }

  // Gére la passe Anfix à Kontix
  def trans(p: S.Program): T.Program = {
    val immuetableList = List()
    val cleanProgram: List[S.Definition] = mainToLet(p)
    val (defs, tailexpr) = compile_definitions(cleanProgram, immuetableList)
    T.Program(defs ++ global_kont, tailexpr)
  }

  var _count = 0

  def generateLabel(): String = {
    val new_label = "k" + _count
    _count += 1
    new_label
  }

  // Compile les définitions Anfix
  def compile_definitions(
      p: S.Program,
      nestedEnv: List[String]
  ): (List[T.Definition], T.TailExpr) = {
    p match {
      case Nil => (List(), T.Ret(T.Num(0)))
      case S.Val(id, e) :: tl =>
        if (!id.equals("_")) {
          throw CustomException("This should Not Happen")
        }
        val recursive_res = compile_definitions(tl, nestedEnv)
        (recursive_res._1, compile_expr_to_tail(e))
      case S.Def(fid, args, e) :: tl =>
        val tail_expr = compile_expr_to_tail(e)
        val def_fun = T.DefFun(fid, args, tail_expr)
        val recursive_res = compile_definitions(tl, nestedEnv)
        (def_fun :: recursive_res._1, recursive_res._2)
    }
  }

  def simple_list_to_basic_list(l: List[S.SimplExpr]): List[T.BasicExpr] = {
    l.map(e => compile_simple_to_basic(e))
  }

  // Permet de compiler une expression en basicExpr si possible
  def compile_expr_to_basic(e: S.Expr): Option[T.BasicExpr] = {
    e match {
      case S.Simple(e) => Some(compile_simple_to_basic(e))
      case S.Let(id, e1, e2) =>
        val op_basic_e1 = compile_expr_to_basic(e1)
        val op_basic_e2 = compile_expr_to_basic(e2)
        (op_basic_e1, op_basic_e2) match {
          case (Some(basic_e1), Some(basic_e2)) =>
            Some(T.BLet(id, basic_e1, basic_e2))
          case _ => None
        }
      case S.If(c, e1, e2) =>
        val (comp, se1, se2) = c
        val basic_se1 = compile_simple_to_basic(se1)
        val basic_se2 = compile_simple_to_basic(se2)
        val op_basic_e1 = compile_expr_to_basic(e1)
        val op_basic_e2 = compile_expr_to_basic(e2)
        val kontix_comp = (comp, basic_se1, basic_se2)
        (op_basic_e1, op_basic_e2) match {
          case (Some(basic_e1), Some(basic_e2)) =>
            Some(T.BIf(kontix_comp, basic_e1, basic_e2))
          case _ => None
        }
      case S.Op(o, e1, e2) =>
        val basic_e1 = compile_simple_to_basic(e1)
        val basic_e2 = compile_simple_to_basic(e2)
        Some(T.Op(o, basic_e1, basic_e2))
      case S.Prim(o, args) =>
        var basic_args = simple_list_to_basic_list(args)
        Some(T.Prim(o, basic_args))
      case S.Call(_, _) => None
    }
  }

  def compile_expr_to_tail(e: S.Expr): T.TailExpr = {
    e match {
      case S.Simple(e) => T.Ret(compile_simple_to_basic(e))
      case S.Let(id, e1, e2) =>
        val op_basic_e1 = compile_expr_to_basic(e1)
        val op_basic_e2 = compile_expr_to_basic(e2)
        (op_basic_e1, op_basic_e2) match {
          case (Some(basic_e1), Some(basic_e2)) =>
            T.Ret(T.BLet(id, basic_e1, basic_e2))
          case (Some(basic_e1), None) =>
            T.Let(id, basic_e1, compile_expr_to_tail(e2))
          case (None, None) =>
            val cont_name = generateLabel()
            val tail_e1 = compile_expr_to_tail(e1)
            val tail_e2 = compile_expr_to_tail(e2)
            //val saves = get_variables_from_tail_expr(tail_e2).distinct
            val saves = get_variables_from_expr(e2).distinct
            val filtered_saves = saves.filter(_ != id)
            global_kont ++= List(
              T.DefCont(cont_name, filtered_saves, id, tail_e2)
            )
            T.PushCont(cont_name, filtered_saves, tail_e1)
          case (None, Some(basic_e2)) =>
            val cont_name = generateLabel()
            val tail_e1 = compile_expr_to_tail(e1)
            //val saves = get_variables_from_basic_expr(basic_e2).distinct
            val saves = get_variables_from_expr(e2).distinct
            val filtered_saves = saves.filter(_ != id)
            val tail_e2 = T.Ret(basic_e2)
            global_kont ++= List(
              T.DefCont(cont_name, filtered_saves, id, tail_e2)
            )
            T.PushCont(cont_name, filtered_saves, tail_e1)
        }
      /*
        val cont_name = generateLabel()
        val saves = get_variables_from_tail_expr(tail_e2)
          if(contains_call(tail_e2)) {
            T.PushCont(cont_name, saves, tail_e2)
          } else {
            T.Let(id, tail_e1, tail_e2)
          }
       */
      // throw CustomException("This should not happen")

      //throw CustomException("This should not happen")

      case S.If(c, e2, e3) =>
        val (comparator, simple1, simple2) = c
        val (basic1, basic2) =
          (compile_simple_to_basic(simple1), compile_simple_to_basic(simple2))
        T.If(
          (comparator, basic1, basic2),
          compile_expr_to_tail(e2),
          compile_expr_to_tail(e3)
        )
      case S.Op(o, e1, e2) =>
        val eg = compile_simple_to_basic(e1)
        val ed = compile_simple_to_basic(e2)
        T.Ret(T.Op(o, eg, ed))
      /* Richard */
      case S.Prim(o, args) =>
        val simple_args = simple_list_to_basic_list(args)
        T.Ret(T.Prim(o, simple_args))
      case S.Call(f, args) =>
        val f_basic = compile_simple_to_basic(f)
        val args_basic = simple_list_to_basic_list(args)
        T.Call(f_basic, args_basic)
    }
  }

  /*
  def get_variables_from_tail_expr(tail_expr: T.TailExpr): List[T.Ident] = {
    tail_expr match {
      case T.Let(id, be, te) =>
        val variables = get_variables_from_basic_expr(be) ++ get_variables_from_tail_expr(te)
        if (id == "_") {
          variables
        } else {
          id :: variables
        }
      case T.If((_, be1, be2), te1, te2) =>
        get_variables_from_basic_expr(be1) ++ get_variables_from_basic_expr(be2) ++
        get_variables_from_tail_expr(te1) ++ get_variables_from_tail_expr(te2)
      case T.Call(be, args) =>
        get_variables_from_basic_expr(be) ++
          args.foldLeft(List[T.Ident]()) { (acc, elt) =>
            acc ++ get_variables_from_basic_expr(elt)
          }
      case T.Ret(be) =>
        get_variables_from_basic_expr(be)
      case T.PushCont(_, saves, te) =>
        saves ++ get_variables_from_tail_expr(te)
    }
  }

  def get_variables_from_basic_expr(basic_expr: T.BasicExpr): List[T.Ident] = {
    basic_expr match {
      case T.Var(id) => List(id)
      case T.BLet(id, be1, be2) =>
        val variables = get_variables_from_basic_expr(be1) ++ get_variables_from_basic_expr(be2)
        if (id == "_") {
          variables
        } else {
          id :: variables
        }
      case T.BIf((c, se1, se2), be1, be2) =>
        get_variables_from_basic_expr(se1) ++ get_variables_from_basic_expr(se2) ++
        get_variables_from_basic_expr(be1) ++ get_variables_from_basic_expr(be2)
      case T.Op(_, be1, be2) =>
        get_variables_from_basic_expr(be1) ++ get_variables_from_basic_expr(be2)
      case T.Prim(_, args) =>
        args.foldLeft(List[T.Ident]()) { //à factoriser en une fonction
          (acc, elt) => acc ++ get_variables_from_basic_expr(elt)
        }
      case _ => List()
    }
  }
   */

  // Récupére les Variables des Expressions
  def get_variables_from_expr(e: S.Expr): List[S.Ident] = {
    e match {
      case S.Simple(e) => get_variables_from_simple(e)
      case S.Let(id, e1, e2) =>
        get_variables_from_expr(e1) ++ get_variables_from_expr(e2).filter(
          _ != id
        )
      case S.If((_, se1, se2), e2, e3) =>
        get_variables_from_simple(se1) ++ get_variables_from_simple(se2) ++
          get_variables_from_expr(e2) ++ get_variables_from_expr(e3)
      case S.Op(_, se1, se2) =>
        get_variables_from_simple(se1) ++ get_variables_from_simple(se2)
      case S.Prim(_, args) =>
        args.foldLeft(List[T.Ident]()) { //à factoriser en une fonction
          (acc, elt) => acc ++ get_variables_from_simple(elt)
        }
      case S.Call(f, args) =>
        get_variables_from_simple(f) ++ args
          .foldLeft(List[T.Ident]()) { //à factoriser en une fonction
            (acc, elt) => acc ++ get_variables_from_simple(elt)
          }
    }
  }

  // Récupére une variable a partir d'une SimpleExpr si possible
  def get_variables_from_simple(se: S.SimplExpr): List[S.Ident] = {
    se match {
      case S.Var(id) => List(id)
      case _         => List()
    }
  }

  // Gére le cas des Simple
  def compile_simple_to_basic(e: S.SimplExpr): T.BasicExpr = {
    e match {
      case S.Num(n)   => T.Num(n)
      case S.Str(s)   => T.Str(s)
      case S.Fun(fid) => T.Fun(fid)
      case S.Var(id)  => T.Var(id)
    }
  }

}
