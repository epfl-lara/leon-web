package leon.web
package websitebuilder
package stringModification

import leon.LeonContext
import leon.purescala.{Common, ExprOps}
import leon.purescala.Common._
import leon.purescala.Definitions.{CaseClassDef, Program, FunDef}
import leon.purescala.Expressions.{AsInstanceOf, CaseClass, CaseClassSelector, Expr, StringConcat, StringLiteral, Tuple, TupleSelect}
import leon.purescala.Types.CaseClassType
import leon.solvers.string.StringSolver
import leon.solvers.string.StringSolver._
import leon.synthesis.FileInterface
import leon.utils.Position
import leon.webDSL.webDescription.{Element, TextElement, WebAttribute, WebElement}
import logging.OptionValWithLog
import logging.serverReporter.{Debug, Info, ServerReporter}
import memory.Memory
import programEvaluator.{SourceMap, TupleSelectAndCaseClassSelectRemover}
import shared._
import programEvaluator.ProgramEvaluator
import scala.collection.mutable.ListBuffer
import leon.utils.RangePosition
import leon.web.shared.messages.SubmitSourceCodeResult
import leon.purescala.DefOps
import leon.web.workers.WebBuilderWorker
import leon.web.models.CompilationState

/**
  * Created by dupriez on 4/18/16.
  */
object StringModificationProcessor {
//
//  /* Evaluates partially an expression until there is no more TupleSelect and CaseClassSelector.*/
//  def simplifyCaseSelect(expr: Expr): Expr = expr match {
//    case TupleSelect(Tuple(args), i) =>
//      args(i - 1)
//    case TupleSelect(arg, i) =>
//      simplifyCaseSelect(TupleSelect(simplifyCaseSelect(arg), i))
//    case CaseClassSelector(cct, CaseClass(ct, args), id) =>
//      args(cct.classDef.selectorID2Index(id))
//    case CaseClassSelector(cct, AsInstanceOf(expr, ct), id) =>
//      simplifyCaseSelect(CaseClassSelector(cct, expr, id))
//    case CaseClassSelector(cct, inExpr, id) =>
//      simplifyCaseSelect(CaseClassSelector(cct, simplifyCaseSelect(inExpr), id))
//    case _ => throw new Exception(s"Cannot partially evaluate $expr")
//  }

  object TupleSelectOrCaseClassSelect {
    def unapply(expr: Expr): Option[Expr] = {
      expr match {
        case TupleSelect(Tuple(args), i) =>
          Some(args(i - 1))
        case TupleSelect(arg, i) =>
          unapply(arg).flatMap(arg2 => unapply(TupleSelect(arg2, i)))
        case CaseClassSelector(cct, CaseClass(ct, args), id) =>
          Some(args(cct.classDef.selectorID2Index(id)))
        case CaseClassSelector(cct, AsInstanceOf(expr, ct), id) =>
          unapply(CaseClassSelector(cct, expr, id))
        case CaseClassSelector(cct, inExpr, id) =>
          unapply(inExpr).flatMap(inExpr2 => unapply(CaseClassSelector(cct, inExpr2, id)))
        case _ => None/* throw new Exception(s"Cannot partially evaluate $expr")*/
      }
    }
  }

  case class StringModificationProcessingException(msg:String) extends java.lang.Exception(msg, null)
   def failure(failureMessage: String) = {
    println(failureMessage)
     throw new StringModificationProcessingException(s"Failure in StringModificationProcessor: ${failureMessage}")
   }

  private def getWebElemAndUnevalExprOfWebElemFromSourceMap(weID: Int, sourceMap: SourceMap, sReporter: ServerReporter) = {

    val webElem = sourceMap.webElementIDToWebElement(weID) match {
      case OptionValWithLog(Some(value), log) =>
        sReporter.report(Info, s"Obtained the webElement (id=$weID) from SourceMap: $value")
        value
      case OptionValWithLog(None, log) =>
        sReporter.report(Info, s"SourceMap query for the webElement (id=$weID) failed")
        failure("SourceMap query for the Expr of the unevaluated webElement failed")
    }

    val unevalExprOfWebElem = sourceMap.webElementIDToExpr(weID) match {
      case OptionValWithLog(Some(value), log) =>
        sReporter.report(Info, s"Obtained the Expr of the unevaluated webElement (id=$weID) from SourceMap: "+ "DISABLED (to re-enable it, look for \"#VERBOSITY\" in StringModificationProcessor.scala)")
//        #VERBOSITY
//        sReporter.report(Info, s"Obtained the Expr of the unevaluated webElement (id=$weID) from SourceMap: $value")
        value
      case OptionValWithLog(None, log) =>
        sReporter.report(Info, "SourceMap query for the Expr of the unevaluated webElement failed")
        failure("SourceMap query for the Expr of the unevaluated webElement failed")
    }

    (webElem, unevalExprOfWebElem)
  }

  def process(w: WebBuilderWorker, cstate: CompilationState, strMod: StringModification, sourceId: Int, serverReporter: ServerReporter) : StringModificationSubmissionResult = {
    val sReporter = serverReporter.startProcess("String Modification Processor")

    val (weID, modWebAttr, newVal) = strMod match {case StringModification(w, m, n)=> (w,m,n)}
    sReporter.report(Info,
      s"""String modification currently processed:
        | webElementID: $weID
        | Modified WebAttribute: $modWebAttr
        | New value: $newVal
      """.stripMargin)

    val result = try{

    }
    catch {
      case StringModificationProcessingException(msg) =>
        StringModificationSubmissionResult(None, msg)
    }

    val sourceMap = Memory.getSourceMap(sourceId).getOrElse(failure(s"Could not find source maps for request $sourceId"))
    val (webElement, unevalExprOfWebElem) = getWebElemAndUnevalExprOfWebElemFromSourceMap(weID, sourceMap, sReporter)
    val sourceCode = sourceMap.sourceCode
    val newSourceId = sourceId + 1 // For the source code after the modification.
    
    val cb = new ProgramEvaluator.ConversionBuilder(sourceMap, serverReporter)
    import cb._

    val (textExpr, originalText) = (webElement, modWebAttr) match {
      case (TextElement(txt), None) =>
         val textExpr = unevalExprOfWebElem match {
           case CaseClass(CaseClassType(_, _), argSeq) => argSeq(0)
        }
        (textExpr, txt)
      case (e: Element, Some(prop)) =>
         val textExpr = (unevalExprOfWebElem match {
           case CaseClass(CaseClassType(_, _), List(tag, children, attributes)) =>
             (exprOfLeonListOfExprToLeonListOfExpr(attributes) find(x => x match {
               case CaseClass(CaseClassType(_, _), List(attrName, attrValue)) =>
                 attrName == prop
               case _ => false
             })) map {
               case CaseClass(CaseClassType(_, _), List(attrName, attrValue)) => attrValue
             }
        }) getOrElse (StringLiteral(""))
        (textExpr, e.attr(prop).getOrElse(""))
      case (we, prop) => failure(s"WebElement $webElement was requested attribute $prop to modify according to the pattern matching in StringModificationProcessor")
    }
    
    /* Takes an expression which can simplify to a single string.
     * Returns an assignment of each of its constants to a fresh and unique ID */
    def textExprToStringFormAndAssignmentMap(textExpr: Expr, assignmentMap: Map[Identifier, String]=Map(), posToId: Map[Position, Identifier]=Map()): (StringForm, Map[Identifier, String], Map[Position, Identifier]) = {
      val actualTextExpr = TupleSelectAndCaseClassSelectRemover.removeTopLevelTupleSelectsAndCaseClassSelects(textExpr)
      actualTextExpr match {
        case StringLiteral(string) =>
          actualTextExpr.getPos.file.getName match { // Is there a better way to check if the constant comes from the library?
            case "WebBuilder.scala" =>
              //This StringLiteral comes from the code of WebBuilder.scala and not from the user's source code,
              // so it is not to be modified by a user StringModification and should therefore be seen as a constant String by StringSolver
              (List(Left(string)), assignmentMap, posToId)
            case _ =>
              //  This StringLiteral comes from the user's source code, so it can be modified by a user StringModification.
              // We generate an identifier for it so that StringSolver is able to assign a new value to this StringLiteral.
              if(actualTextExpr.getPos.line == -1) throw new Exception("Line is -1 on " + actualTextExpr)
              val identifier = posToId.getOrElse(actualTextExpr.getPos, Common.FreshIdentifier("l:"+actualTextExpr.getPos.line+",c:"+actualTextExpr.getPos.col).copiedFrom(actualTextExpr))
              println("Creating identifier " +identifier + " -> file " + actualTextExpr.getPos.file.getName + " \"" + string + "\"")
              (List(Right(identifier)), assignmentMap + (identifier -> string), posToId+(actualTextExpr.getPos -> identifier))
          }
        case StringConcat(tExpr1, tExpr2) =>
          textExprToStringFormAndAssignmentMap(tExpr1, assignmentMap, posToId) match {
          case (strForm1, assignMap1, posToID1) =>
            textExprToStringFormAndAssignmentMap(tExpr2, assignMap1, posToID1) match {
              case (strForm2, assignMap2, posToID2) =>
                (strForm1 ++ strForm2, assignMap2, posToID2)
            }
        }
      }
    }

    sReporter.report(Info, "Original text= " + originalText)
    val (stringForm, assignmentMap, _) = textExprToStringFormAndAssignmentMap(textExpr)
    sReporter.report(Info, "StringForm= " + stringForm)
    val problem: Problem = List((stringForm, newVal))
    sReporter.report(Info, "StringSolver problem: "+StringSolver.renderProblem(problem))
//    val solutionStream: Stream[Assignment] = StringSolver.solve(problem)
    val solutions = solveMinChange(problem, assignmentMap)
    sReporter.report(Info, "First 15 StringSolver solutions:")
    val ssReporter = sReporter.addTab
    solutions.take(15).foreach(assignment => ssReporter.report(Info, assignment.toString))
//    solutions.take(15).foldLeft("")((str, assignment)=>str+assignment.toString()).foreach(solution => ssReporter.report(Info, ))
    val firstSol = solutions.headOption match{
      case Some(value) => value
      case None => failure("StringSolver returned no solutions")
    }
    /** Pour chaque solution, on duplique le programme original, en effectuant le remplacement prescris par la solution
      * Pour chaque solution, on duplique le programme original, en effectuant le remplacement prescris par la solution
      * On execute ces programmes  pour obtenir un Stream de webpage: SW
      * On envoie la premiere WebPage au client pour qu'il l'affiche dans le viewer (et qu'il la stocke)
      * On prend la deuxieme webPage, on la compare a la premiere -> ChangeList (List[TextNode de la premiere WebPage, TextNode correspondant de la deuxieme WebPage]) : CL
      * On envoie au client la deuxieme webPage et CL
      * Le client cree un menu deroulant dans Disambiguator, en ajoutant un item pour la deuxieme webPage
      * On la troisieme webpage, on la compare
      *
      *
      */
//    import leon.purescala.DefOps
//    val p : Program = _
////    Do not replace top-level vals
//    val newP = DefOps.replaceFunDefs(p)({ fd =>
//      if(ExprOps.exists(e => firstSol.exists({case (id, str) => id.getPos==e.getPos}))(fd.fullBody)){
//        val newFD = fd.duplicate()
//        newFD.fullBody = ExprOps.preMap(e => firstSol.find({case (id, str) => id.getPos==e.getPos}) match {
//          case Some((id,str)) =>
//            Some(StringLiteral(str))
//          case None => None
//        })(fd.fullBody)
//        Some(newFD)
//      }
//      else None
//    })._1

    implicit val context = LeonContext.empty
    val fileInterface = new FileInterface(context.reporter)
//    var changedElements = List[StringPositionInSourceCode]()

    def applySolution(sourceCode: String, program: Program, solution: StringSolver.Assignment): (String, Program, Option[FunDef]) = {
      println("Transforming a solution...")
      solution.toList
        .sortBy({case (identifier, str) => identifier.getPos})
        .reverse
        .foldLeft((sourceCode, program, ProgramEvaluator.functionToEvaluate))(
          {case ((sCode, prog, optFunDef), (identifier, string)) =>
            val replacement = StringLiteral(string)
            val (newProg, _, newFuns, _) = DefOps.replaceDefs(prog)(fd => {
             if(ExprOps.exists{ e => e.getPos == identifier.getPos }(fd.fullBody)) {
               val newFd = fd.duplicate()
               newFd.fullBody = ExprOps.postMap{
                 case e if e.getPos == identifier.getPos =>
                   Some(replacement)
                 case _ => None
                 }(fd.fullBody)
               Some(newFd)
             } else None
           }, cd => None)
          (fileInterface.substitute(sCode, identifier, replacement),
           newProg,
           optFunDef.flatMap(newFuns.get)
          )}
        )
    }

    def buildListOfStringPositionsForModifiedIdentifier(solution: StringSolver.Assignment): List[StringPositionInSourceCode] = {
      solution.map({case (identifier, str)=>
        identifier.getPos match {
          case RangePosition(lineFrom, colFrom, pointFrom, lineTo, colTo, pointTo, file) => StringPositionInSourceCode(lineFrom, colFrom, lineTo, colTo)
          case _ => StringPositionInSourceCode(0, 0, 0, 0)
        }
      }).toList
    }

    val (newSourceCode, newProgram, newFunDef) = applySolution(sourceCode, cstate.program, firstSol)
    val changedElements = buildListOfStringPositionsForModifiedIdentifier(firstSol)


//    val newSourceCode = firstSol.toList
////      Apply the modifications from the bottom to the top, to keep the line numbers consistent for the next modifications.
//      .sortBy({case (identifier, str) => identifier.getPos})
//      .reverse
//      .foldLeft(sourceCode)(
//        {case (sCode, (identifier, string))=>
//          changedElements = (identifier.getPos match {
//            case RangePosition(lineFrom, colFrom, pointFrom, lineTo, colTo, pointTo, file) => StringPositionInSourceCode(lineFrom, colFrom, lineTo, colTo)
//            case _ => StringPositionInSourceCode(0, 0, 0, 0)
//          }) :: changedElements
//          fileInterface.substitute(sCode, identifier, StringLiteral(string))}
//      )
    sReporter.report(Info, "New source code: "+ "DISABLED (to re-enable it, look for \"#VERBOSITY\" in StringModificationProcessor.scala)")
//    #VERBOSITY
//    sReporter.report(Info, "New source code: "+ newSourceCode)
    //val apiService = new ApiService(onUserRequest = false)
    sReporter.report(Info, "Submitting the new source code (as if the client did it)")
    
    val newCState = CompilationState(
        code = Some(newSourceCode),
        project = cstate.project,
        savedFile = cstate.savedFile,
        compResult = "success",
        optProgram = Some(newProgram),
        requestId = cstate.requestId,
        // Imperative information
        wasLoop = cstate.wasLoop
    )
    w.processNewCode(newCState, false, newFunDef) match {
      case SubmitSourceCodeResult(SourceCodeSubmissionResult(Some(webPageIDed), _), newSourceId) =>
        sReporter.report(Info, "Sending back to client the new source code and a WebPage with IDed WebElements")
        StringModificationSubmissionResult(Some(StringModificationSubmissionConcResult(newSourceCode, changedElements, newSourceId, webPageIDed)), "")
      case SubmitSourceCodeResult(SourceCodeSubmissionResult(None, log), newSourceId) =>
        sReporter.report(Info, "The submission of the new source code failed because: "+log)
        StringModificationSubmissionResult(None, log)
    }
  }
}

//To be stored and reused along with the client-server dialogue on the resolving of a string modification ambiguity
//case class AmbiguityResolvingSession(originalSourceCode: String, originalStringModification: StringModification, lastQuestionAsked: AmbiguityResolvingQuestion, solutionStream: Stream[StringSolver.Assignment])
