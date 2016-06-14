package leon
package web
package utils

import leon.purescala.DefOps
import leon.synthesis.FileInterface
import models.MuteReporter
import leon.LeonContext
import leon.purescala.ScalaPrinter
import leon.purescala.Definitions.FunDef
import leon.purescala.PrinterOptions
import models.CompilationState
import leon.purescala.PrinterContext
import models.MuteReporter
import leon.purescala.ExprOps

/**
 * @author Mikael
 */
object FileInterfaceWeb {
  def allCodeWhereFunDefModified(fd: FunDef)(modifyCopy: FunDef => Unit)(implicit cstate: CompilationState, context: LeonContext): String = {
    import leon.purescala.PrinterHelpers._
    val fInt = new FileInterface(new MuteReporter())

    val nfd = fd.duplicate()
    
    modifyCopy(nfd)

    val fds = nfd :: Nil

    val transformer =  DefOps.funDefReplacer(f => if(f == fd) Some(nfd) else None)
    val prog = DefOps.transformProgram(transformer, cstate.program)
    val p = new ScalaPrinter(PrinterOptions(), Some(prog))

    val allCode = fInt.substitute(cstate.code.getOrElse(""),
                                  fd,
                                  (indent) => {
      implicit val pctx = PrinterContext(fd, Nil, indent, p)
      p"${nfd}"
      p.toString
    })

    allCode
  }
  def allCodeWhereFunDefAdded(after: FunDef)(toAdd: FunDef*)(implicit cstate: CompilationState, context: LeonContext): String = {
    import leon.purescala.PrinterHelpers._
    val fInt = new FileInterface(new MuteReporter())

    val prog = DefOps.addFunDefs(cstate.program, toAdd, after)
    val p = new ScalaPrinter(PrinterOptions(), Some(prog))

    val allCode = fInt.insertAfter(cstate.code.getOrElse(""),
                                   after,
                                   (indent) => {
      implicit val pctx = PrinterContext(after, DefOps.pathFromRoot(after)(prog), indent, p)
      for(fd <- toAdd)
        p"""|
            |
            |${fd}"""
      p.toString
    })

    allCode
  }
}