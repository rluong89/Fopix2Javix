/* Trac main : command-line arguments + launch of compilations */

package trac

import trac.transl._

object Trac extends App {
  val run = !args.contains("-nointerp")
  val trace = args.contains("-trace")
  val files = args.filter{s => s.take(1) != "-"}
  files.foreach { file =>
    println("\nHandling " + file)
    fopix.Parse.parseFile(file).foreach {doFopixAst(file,_,run,trace)}
  }

  def doFopixAst
       (file:String,ast:fopix.AST.Program,run:Boolean,trace:Boolean) : Unit = {
    val fileNoExt = file.take(file.lastIndexOf("."))
    val basename = if (fileNoExt == "") file else fileNoExt

    //If you want to print the ast of this fopix program, uncomment this:
    //println("Fopix AST : " + ast)

    // Reprint the parsed ast in foo.fopix
    val fopixfile = basename + ".fopix"
    if (file != fopixfile) {
      println("Fopix file reprint : " + fopixfile)
      writeFile(fopixfile,fopix.PP.pp(ast))
    }
    if (run) print("Fopix interp :\n" + fopix.Interp.eval(ast))

    // Direct Javix compilation (in foo.j)
    val javixAst = Fopix2Javix.compile(basename,ast)
    val javixfile = basename + ".j"
    println("Javix file : " + javixfile)
    writeFile(javixfile,javix.PP.pp(javixAst))
    if (run) print("Javix interp :\n" + javix.Interp.eval(javixAst,trace))
  }

  def writeFile(file:String,content:String) {
    import java.io._
    val pw = new PrintWriter(new File(file))
    pw.print(content)
    pw.close()
  }
}
