
/* This module implements a compiler from Fopix to Javix. */

// TODO : To finish !!

package trac.transl

object Fopix2Javix {

import trac._
import trac.PrimOp._
import trac.BinOp._
import trac.fopix.{AST=>S}
import trac.javix.{AST=>T}

/* /!\ Attention, le code suivant n'est qu'une suggestion pour commencer.
 * Il est probablement à remanier encore (structures en plus, arguments
 * supplémentaires, ...) */

def compile (progname:String,p:S.Program) : T.Program = {
  val varsize = 100
  val stacksize = 10000
  //val instrs = List(T.Comment("Todo!!"),T.Return)
  val instrs = compile_definitions(p) ++ List(T.Return)
  T.Program(progname,instrs,varsize,stacksize)
}

def compile_definitions (p:List[S.Definition]) : List[T.Instruction] = {
  /* TODO: à completer, on ne s'occupe ici que du premier Val ! */
  p match {
    case Nil => List()
    case S.Val(x,e) :: p => compile_expr(e)
    case S.Def(_,_,_) :: p => compile_definitions(p)
  }
}

/* TODO: ajouter une structure d'environnement des variables,
 * du genre Map[S.Ident,T.Var] donnant le numéro de la variable Javix
 * correspondant à un nom de variable Fopix */

/* Invariant :
 *  A l'exécution, le code généré par compile_expr(e) aura l'effet suivant :
 *    pile ==> pile,v  avec v la valeur résultat de l'évaluation de e
 */

def compile_expr(e:S.Expr) : List[T.Instruction] = {
  e match {
    case S.Num(n) => List(T.Push(n))
    case S.Str(s) => List(T.Ldc(s))
    /* case Var(v) => List(T.ILoad(...))
     * où le numéro vient d'une recherche dans la future table des variables */
    case S.Op(o,e1,e2) => 
      /* code si o est un opérateur arithmetique
       * TODO: que faire si o est une comparaison ? */
      compile_expr(e1)++compile_expr(e2)++List(T.IOp(BinOp.toArith(o)))
    /* Un exemple de primitif : le print_int */
    case S.Prim(Printint,List(e1)) =>
      compile_expr(e1)++List(T.IPrint,T.Push(0))
      /* Push(0) correspond au résultat de type unit du print_int */
    case _ => List() // TODO : traiter tous les cas manquants !
  }
}


}
