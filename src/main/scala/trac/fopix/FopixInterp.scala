// Interpretation of Fopix programs

// TODO : To finish !

package trac.fopix

object Interp {

  import scala.annotation.tailrec
  import scala.collection.mutable.{Map => MutableMap}
  import trac._
  import trac.fopix.AST._
  import trac.PrimOp._

  type Address = Int

  type Block = Array[Result]
  type Memory = MutableMap[Address, Block]
  type FunEnv = Map[FunIdent, (List[Ident], Expr)]
  type Env = Map[Ident, Result] // immutable map of variables

  sealed abstract class Result {
    // Customized pretty-print of interpretation results
    override def toString: String =
      this match {
        case RUnit         => "()"
        case RInt(n)       => n.toString
        case RStr(s)       => "\"" + s + "\""
        case RBool(b)      => b.toString
        case RBlk(a)       => "@" + a.toString
        case RFun(f, args) => f + args.mkString("(", ",", ")")
      }
  }

  case object RUnit extends Result
  case class RInt(n: Int) extends Result
  case class RStr(s: String) extends Result
  case class RBool(b: Boolean) extends Result
  case class RBlk(a: Address) extends Result
  case class RFun(f: FunIdent, args: List[Result]) extends Result

  def getInt(r: Result, msg: String): Int =
    r match {
      case RInt(n) => n
      case _       => throw new Invalid(msg + " is not an integer")
    }
  def getBool(r: Result, msg: String): Boolean =
    r match {
      case RBool(n) => n
      case _        => throw new Invalid(msg + " is not a boolean")
    }

  def getFun(r: Result): FunIdent =
    r match {
      case RFun(f, _) => f
      case _          => throw new Invalid("Can't find function")
    }

// NB: in RFun, args is empty unless you try to accept partial application
// and over-applications

// Global mutable elements for interpretation :

  var allPrints: List[String] = List()
  var memsize = 0
  val mem: Memory = MutableMap.empty
  var count = 0

  def reset(): Unit = {
    allPrints = List()
    memsize = 0
    mem.clear()
  }

  def eval(p: Program): String = {
    reset()
    val initEnv: Env = Map.empty
    val initFunEnv: FunEnv = Map.empty
    eval(initEnv, initFunEnv, p)
    StringContext.processEscapes(allPrints.reverse.mkString)
  }

  def evalFun(funEnv: FunEnv, p: Program): FunEnv =
    p match {
      case Nil            => funEnv
      case Val(_, _) :: p => evalFun(funEnv, p)
      case Def(fid, args, e) :: p =>
        val newFunEnv = funEnv + (fid -> (args, e))
        evalFun(newFunEnv, p)
    }

  def eval(env: Env, funEnv: FunEnv, p: Program): Env =
    p match {
      case Nil            => env
      case Val(x, e) :: p => eval(env + (x -> eval(env, funEnv, e)), funEnv, p)
      case Def(fid, args, e) :: p =>
        val newFunEnv = funEnv + (fid -> (args, e))
        eval(env, newFunEnv, p)
    }

  def eval(env: Env, funEnv: FunEnv, e: Expr): Result =
    e match {
      case Num(n) => RInt(n)
      case Str(s) => RStr(s)
      case Var(v) => env(v)
      case Fun(f) => RFun(f, List())
      case Op(o, e1, e2) =>
        binop(o, eval(env, funEnv, e1), eval(env, funEnv, e2))
      case If(e1, e2, e3) =>
        val e1_value = eval(env, funEnv, e1)
        if (getBool(e1_value, "e1")) {
          eval(env, funEnv, e2)
        } else {
          eval(env, funEnv, e3)
        }
      case Let(x, e1, e2) =>
        val new_env = env + (x -> eval(env, funEnv, e1))
        eval(new_env, funEnv, e2)
      case Prim(p, l) => prim(p, l.map(eval(env, funEnv, _)))
      case Call(expr, l) =>
        val fid = getFun(eval(env, funEnv, expr))
        val funInfoOption = funEnv.get(fid)
        val (argsList, e) = funInfoOption match {
          case Some((args, e)) => (args, e)
          case None            => throw new Invalid("Can't find function: " + fid)
        }
        val zippedList = l zip argsList
        val newValEnv = zippedList.foldLeft(env)((accEnv, matchArgs) => {
          val evalArg = eval(env, funEnv, matchArgs._1)
          accEnv + (matchArgs._2 -> evalArg)
        })
        eval(newValEnv, funEnv, e)
    }

  def binop(o: BinOp.T, r1: Result, r2: Result): Result = {
    val msg = "Binop argument"
    val n1 = getInt(r1, msg)
    val n2 = getInt(r2, msg)
    if (BinOp.isArith(o))
      RInt(IntOp.eval(BinOp.toArith(o), n1, n2))
    else
      RBool(CompOp.eval(BinOp.toCmp(o), n1, n2))
  }

  def prim(p: PrimOp.T, args: List[Result]): Result =
    (p, args) match {
      case (Printint, List(RInt(n))) =>
        allPrints = n.toString :: allPrints; RUnit
      case (Printstr, List(RStr(s)))       => allPrints = s :: allPrints; RUnit
      case (Cat, List(RStr(s1), RStr(s2))) => RStr(s1 + s2)
      case (New, List(RInt(i))) =>
        val res = RBlk(count)
        val array: Array[Result] = new Array[Result](i)
        mem += (i -> array)
        count += 1
        memsize += 1
        res
      case (Get, List(RBlk(a), RInt(i))) =>
        val optionalR = mem.get(a)
        optionalR match {
          case None        => throw new Invalid("Array does not exist")
          case Some(array) => array(i)
        }
      case (Set, List(RBlk(array), RInt(index), new_val)) =>
        val optionalArray = mem.get(array)
        optionalArray match {
          case None        => throw new Invalid("Array does not exist")
          case Some(array) => 
            array(index) = new_val
            RUnit
        }
      case (Tuple, _) =>
        val size = args.length
        val res = RBlk(count)
        val array : Array[Result] = new Array[Result](size)
        args.foldLeft(0) {
          (acc, elt) => array(acc) = elt; acc + 1
        }
        mem += (count -> array)
        memsize += 1
        count += 1
        res
      case _ =>
        throw new Invalid("Unsupported primitive call (TODO ? bad arg ?)")
    }

}
