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

  def isVal(e: S.Definition): Boolean = {
    e match {
      case Val(_, _)         => true
      case Def(fid, args, e) => false
    }
  }

  // Fonction qui permettera de compiler le main en suite de let
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

  def trans(p: S.Program): T.Program = {
    val immuetableList = List()
    val cleanProgram: List[S.Definition] = mainToLet(p)
    print(PP.pp(Anfix2Fopix.trans(cleanProgram)))
    val (defs, tailexpr) = compile_definitions(cleanProgram, immuetableList)
    T.Program(defs, tailexpr)
  }

  var _count = 0

  def generateLabel(): String = {
    val new_label = "x" + _count
    _count += 1
    new_label
  }

  def compile_definitions(
      p: S.Program,
      nestedEnv: List[String]
  ): (List[T.Definition], T.TailExpr) = {
    p match {
      case Nil => (List(), T.Ret(T.Num(0)))
      /* Yassine */
      case S.Val(id, e) :: tl =>
        if (!id.equals("_")) {
          throw CustomException("THIS SHOULD NOT HAPPENTTT" + id)
        }
        //Liste immutable ?
        // J'étends l'environnement seulement si nécessaire
        /*val new_list = if (id.equals("_")) { nestedEnv }
        else { id :: nestedEnv }
        // On calcule le reste
        val recursive_res = compile_definitions(tl, new_list)
        // Match pour savoir si on peut traduir en BasicExpr
        e match {
          case S.Simple(e) =>
            (
              recursive_res._1,
              T.Let(id, compile_simple_to_basic(e), recursive_res._2)
            )
          case e =>
            // Generation de la continuation + Lancement de celle-ci
            val generatedLabel = generateLabel()
            // Crée une classe qui fournis des générateur de label ?
            val tailexpr = compile_expr_to_tail(e)
            val defkont = T.DefCont(generatedLabel, nestedEnv, id, tailexpr)
            val pushkont =
              T.PushCont(generatedLabel, nestedEnv, recursive_res._2)
            val new_definitions = defkont :: recursive_res._1
            val new_tailexpr = pushkont
            (new_definitions, new_tailexpr)
        }*/
        val recursive_res = compile_definitions(tl, nestedEnv)
        (recursive_res._1, compile_expr_to_tail(e))

      /* Richard */
      case S.Def(fid, args, e) :: tl =>
        val recursive_res = compile_definitions(tl, nestedEnv)
        recursive_res
    }
  }

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
        var basic_args = args.foldLeft(List[T.BasicExpr]()) { (acc, elt) =>
          acc ++ List(compile_simple_to_basic(elt))
        }
        Some(T.Prim(o, basic_args))
      case S.Call(_, _) => None
    }
  }

  def compile_expr_to_tail(e: S.Expr): T.TailExpr = {
    e match {
      /* pas sur */
      case S.Simple(e) => T.Ret(compile_simple_to_basic(e))
      /* peut optimiser peut être simple */
      /* Yassine */
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
            val saves = get_variables_from_tail_expr(tail_e2)
            T.DefCont(cont_name, saves, id, tail_e2)
            T.PushCont(cont_name, saves, tail_e1)
          case (None, Some(basic_e2)) =>
            val cont_name = generateLabel()
            val tail_e1 = compile_expr_to_tail(e1)
            val saves = get_variables_from_basic_expr(basic_e2)
            val tail_e2 = T.Ret(basic_e2)
            T.DefCont(cont_name, saves, id, tail_e2)
            T.PushCont(cont_name, saves, tail_e1)
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
      /*
        e1 match {
          case Call(f, args) =>
            var cont_name = generateLabel
            var basic_f = compile_simple_to_basic(f)
            var basic_args = args.foldLeft(List[T.BasicExpr]()) {
              (acc, elt) => acc ++ List(compile_simple_to_basic(elt))
            }
            var call = T.Call(basic_f, basic_args)
            var saves = get_variables_from_tail_expr(compile_expr_to_tail(e2))
            T.PushCont(cont_name, saves, call)

        }
       */
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
      case S.Prim(o, args) => handlePrim(o, args)
      case S.Call(f, args) => T.Ret(T.Num(0))
    }
  }

  def get_variables_from_tail_expr(tail_expr: T.TailExpr): List[T.Ident] = {
    tail_expr match {
      case T.Let(id, be, te) =>
        id :: get_variables_from_basic_expr(be) ++ get_variables_from_tail_expr(
          te
        )
      case T.If(_, te1, te2) =>
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
        id :: get_variables_from_basic_expr(
          be1
        ) ++ get_variables_from_basic_expr(be2)
      case T.BIf(_, be1, be2) =>
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

  def handlePrim(o: PrimOp.T, args: List[anfix.AST.SimplExpr]): T.TailExpr = {
    o match {
      case PrimOp.Printint =>
        val liste = args.map(e => compile_simple_to_basic(e))
        T.Ret(T.Prim(o, liste))
      case _ => T.Ret(T.Num(0))
    }

  }

  def compile_simple_to_basic(e: S.SimplExpr): T.BasicExpr = {
    e match {
      case S.Num(n)   => T.Num(n)
      case S.Str(s)   => T.Str(s)
      case S.Fun(fid) => T.Fun(fid)
      case S.Var(id)  => T.Var(id)
    }
  }

}
