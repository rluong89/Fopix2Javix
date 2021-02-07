
/* This module implements a compiler from Kontix to Javix. */

// TODO : To finish !!

package trac.transl

object Kontix2Javix {

import trac._
import trac.PrimOp._
import trac.kontix.{AST=>S}
import trac.javix.{AST=>T}

def compile (progname:String,p:S.Program) : T.Program = {
  val varsize = 100
  val stacksize = 10000
  val instrs = List(T.Comment("Todo!!"),T.Return)
  T.Program(progname,instrs,varsize,stacksize)
}

}
