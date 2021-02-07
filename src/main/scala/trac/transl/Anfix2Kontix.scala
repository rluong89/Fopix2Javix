
/* This module implements the compilation from Anfix to Kontix */

// TODO : To finish !!

package trac.transl

object Anfix2Kontix {

import trac._
import trac.anfix.{AST=>S}
import trac.kontix.{AST=>T}

def trans (p:S.Program) : T.Program = {
  T.Program(List(), T.Ret(T.Str("TODO")))
}

}
