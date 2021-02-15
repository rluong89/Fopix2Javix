// * CPS conversion via examples

/* CPS : continuation-passing-style, i.e. any function
    will receive an extra argument, a continuation, which is
    a function launched when the current function is done.
    In particular, a code converted to CPS is tail-recursive. */

// FIRST EXAMPLE : fact

// Initial code, not tail-recursive

object FactV1 {

def fact(n:Int) : Int = if (n == 0) 1 else fact(n-1) * n

val test = fact(10)

}

/* First CPS version, using nested anonymous functions
   (ok in Scala, but not in Fopix nor Javix) */

object FactV2 {

def fact[T](n:Int, k:Int=>T) : T = {
  if (n == 0) k(1)
  else fact(n-1, r=>k(r*n))
}

val test = fact(10,r=>r) // initial continuation : identity

}

/* Flattened CPS code : no nested anonymous functions anymore,
   but we still need some partial applications to propagate the value
   of some variables right into the body of the created continuation.
   (Scala : ok, but still not Fopix-compatible nor Javix) */

object FactV3 {

def fact[T](n:Int, k:Int=>T) : T = {
 if (n==0) k(1)
 else fact(n-1, kont(k,n))
}
def kont[T](k:Int=>T, n:Int) = { (r:Int) => k(r*n) }

val test = fact(10, r=>r)

}

/* First-order CPS : we regroup the propagated variables into
   a tuple, named "environment". Then, for consistency, all
   functions and continuations take an extra environment arg.

   In practice, an env is a tuple, with a continuation in the
   first place, an older env in second place, and optionally
   some other needed variables after that.

   NB: Having a nicely typed env structure isn't easy, and
   isn't important, so we play dirty here (see Any below and
   the unsafe cast asInstanceOf).
*/

//type Env = (((Int,Env)=>T), Env, Int)

type Env = (Any,Any,Int)
type Kont[T] = (Int,Env)=>T
type ActualEnv[T] = (Kont[T], Env, Int)
def castEnv[T](e:Env) = e.asInstanceOf[ActualEnv[T]]
val initK : Kont[Int] = (r,e)=>r
val initEnv : Env = (0,0,0)       // or null

object FactV4 {

def fact[T](n:Int, k:Kont[T], e:Env) : T = {
  if (n == 0) k(1,e)
  else fact(n-1, kont[T], (k,e,n))
}

def kont[T](r:Int,env:Env) : T = {
  val (k,e,n) = castEnv(env)
  k(r*n,e)
}
val test = fact(10, initK, initEnv)
}

/* First-Order CPS with well-typed environment.
   If we really want, we can define a type of environment
   without unsafe cast in Scala.
   But that's irrelevent for Fopix and Javix. */

object FactV5 {

sealed abstract class Env
type Kont[T] = (Int,Env)=>T
case class PushEnv[T](k:Kont[T], e:Env, n:Int) extends Env

def fact[T](n:Int, k:Kont[T], e:Env) : T = {
  if (n == 0) k(1,e)
  else fact(n-1, kont[T], PushEnv(k,e,n))
}

def kont[T](r:Int,env:Env) : T = {
  env match { case PushEnv(k:Kont[T],e,n) => k(r*n,e) }
}

val test = fact(10, (r:Int,e:Env)=>r, null)

}

// SECOND EXAMPLE : fibonacci

// This will lead to two generated continuations

object FibV1 {
 def fib(n:Int) : Int = if (n <= 1) 1 else fib(n-1) + fib(n-2)
 val test = fib(10)
}

// First CPS version, using nested anonymous functions

object FibV2 {

def fib [T](n:Int, k:Int=>T) : T = {
  if (n <= 1) k(1)
  else
    fib (n-1,
         r1 => fib (n-2,
                    r2 => k (r1+r2)))
}

val test = fib (10, r=>r)
}

// Flattened CPS code, with partial application

object FibV3 {

def fib[T](n:Int, k:Int=>T) : T = {
  if (n <= 1) k(1)
  else fib(n-1, kont1(k,n))
}
def kont1[T] (k:Int=>T,n:Int) = (r1:Int) => fib(n-2, kont2(k,r1))
def kont2[T] (k:Int=>T,r1:Int) = (r2:Int) => k(r1+r2)

val test = fib(10, r=>r)
}

// First-order CPS

object FibV4 {

def fib[T](n:Int, k:Kont[T], e:Env) : T = {
  if (n <= 1) k(1,e)
  else fib(n-1, kont1[T], (k,e,n))
}
def kont1[T](r1:Int,env:Env) : T = {
  val (k,e,n) = castEnv(env)
  fib(n-2, kont2[T], (k,e,r1))
}
def kont2[T](r2:Int,env:Env) : T = {
  val (k,e,r1) = castEnv(env)
  k(r1+r2,e)
}

val test = fib(10, initK, initEnv)

}

// THIRD EXAMPLE : some nested function calls

object G1 {

def g (n:Int) : Int = if (n == 0) 0 else n - g(g(n-1))
val test = g(10)

}

// CPS with nested anonymous functions

object G2 {

def g[T](n:Int, k:Int=>T) : T = {
 if (n == 0) k(0)
 else g(n-1, r1 => g(r1, r2 => k(n-r2)))
}
val test = g(10, x=>x)

}

// CPS with 1st order functions

object G3 {

def g[T](n:Int, k:Kont[T], e:Env) : T = {
 if (n == 0) k(0,e)
 else g (n-1, kont1[T], (k,e,n))
}
def kont1[T](r1:Int, env:Env) : T = {
  g(r1,kont2[T],env)
}
def kont2[T](r2:Int, env:Env) : T = {
  val (k,e,n) = castEnv(env)
  k(n-r2,e)
}

val test = g(10, (r,_)=>r, initEnv)

}

/* NB : all our examples above use a ternary env
   (1 cont + 1 saved env + 1 saved var), but the number
   of saved variables may vary. */
