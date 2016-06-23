package leon.web
package websitebuilder
package stringModification

import leon.LeonContext
import leon.purescala.{Common, ExprOps}
import leon.purescala.Common._
import leon.purescala.Definitions.{CaseClassDef, FunDef, Program}
import leon.purescala.Expressions.{AsInstanceOf, CaseClass, CaseClassSelector, Expr, StringConcat, StringLiteral, Tuple, TupleSelect}
import leon.purescala.Types.CaseClassType
import leon.solvers.string.StringSolver
import leon.solvers.string.StringSolver._
import leon.synthesis.FileInterface
import leon.utils.Position
import leon.webDSL.webDescription._
import logging.OptionValWithLog
import logging.serverReporter.{Error, Info, ServerReporter, _}
import memory.Memory
import programEvaluator.{ProgramEvaluator, SourceMap, _}
//import shared.{PotentialWebPagesList, StringModificationSubmissionResult, _}
import leon.web.shared._

import scala.collection.mutable.ListBuffer
import leon.utils.RangePosition
import leon.web.shared.messages.SubmitSourceCodeResult
import leon.purescala.DefOps
import leon.web.workers.WebBuilderWorker
import leon.web.models.CompilationState
import stringModification.ClarificationSession

import scala.collection.mutable

/**
  * Created by dupriez on 4/18/16.
  */
object StringModificationProcessor {

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
        case _ => None /* throw new Exception(s"Cannot partially evaluate $expr")*/
      }
    }
  }

  case class StringModificationProcessingException(msg: String) extends java.lang.Exception(msg, null)

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
        sReporter.report(Info, s"Obtained the Expr of the unevaluated webElement (id=$weID) from SourceMap: " + "DISABLED (to re-enable it, look for \"#VERBOSITY\" in StringModificationProcessor.scala)")
        //        #VERBOSITY
        //        sReporter.report(Info, s"Obtained the Expr of the unevaluated webElement (id=$weID) from SourceMap: $value")
        value
      case OptionValWithLog(None, log) =>
        sReporter.report(Info, "SourceMap query for the Expr of the unevaluated webElement failed")
        failure("SourceMap query for the Expr of the unevaluated webElement failed")
    }

    (webElem, unevalExprOfWebElem)
  }

  def process(w: WebBuilderWorker, cstate: CompilationState, strMod: StringModification, sourceId: Int, serverReporter: ServerReporter): StringModificationSubmissionResult = {
    val sReporter = serverReporter.startProcess("String Modification Processor")
    val maxNumberOfConsideredSolutions = 2 /*6*/
    //    "Proposed Solutions" are chosen among the "Considered Solutions" so that they do not agree on the first modified text of the webpage
    val maxNumberOfProposedSolutions = 2 /*4*/

    val (weID, modWebAttr, newVal) = strMod match {
      case StringModification(w, m, n) => (w, m, n)
    }
    sReporter.report(Info,
      s"""String modification currently processed:
        | webElementID: $weID
        | Modified WebAttribute: $modWebAttr
        | New value: $newVal
      """.stripMargin)

    val result = try{
      val sourceMap = Memory.getSourceMap(sourceId).getOrElse(failure(s"Could not find source maps for request $sourceId"))
      val (webElement, unevalExprOfWebElem) = getWebElemAndUnevalExprOfWebElemFromSourceMap(weID, sourceMap, sReporter)
      val sourceCode = sourceMap.sourceCode
      val newSourceId = sourceId + 1 // For the source code after the modification.

      val cb = new ProgramEvaluator.ConversionBuilder(sourceMap, serverReporter)
      import cb._

      val (textExpr, originalText) = (webElement, modWebAttr) match {
        case (TextElement(txt), None) =>
          val textExpr = unevalExprOfWebElem match {
            case CaseClass(CaseClassType(_, _), argSeq) => argSeq.head
          }
          (textExpr, txt)
        case (e: Element, Some(prop)) =>
          val textExpr = (unevalExprOfWebElem match {
            case CaseClass(CaseClassType(_, _), List(tag, children, attributes)) =>
              (exprOfLeonListOfExprToLeonListOfExpr(attributes) find (x => x match {
                case CaseClass(CaseClassType(_, _), List(attrName, attrValue)) =>
                  attrName == prop //TODO: Intellij detects a comparison between elements of 2 different types
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
      def textExprToStringFormAndAssignmentMap(
                                                textExpr: Expr,
                                                serverReporter: ServerReporter
                                              ): (StringForm, Map[Identifier, String], Map[Position, Identifier]) = {
        val sReporter = serverReporter.startFunction("textExprToStringFormAndAssignmentMap")
        sReporter.report(Info, "Called on TextExpr: " + textExpr)
        //        Recursive function that will compute the result
        def recFunction(
                         textExpr: Expr,
                         serverReporter: ServerReporter,
                         assignmentMap: Map[Identifier, String] = Map(),
                         posToId: Map[Position, Identifier] = Map()
                       ): (StringForm, Map[Identifier, String], Map[Position, Identifier]) = {
          val sReporter = serverReporter.addTab
          sReporter.report(Info, "Processing: " + textExpr)
          val actualTextExpr = TupleSelectAndCaseClassSelectRemover.removeTopLevelTupleSelectsAndCaseClassSelects(textExpr)
          sReporter.report(Info, "actualTextExpr: " + actualTextExpr)
          actualTextExpr match {
            case StringLiteral(string) => actualTextExpr.getPos match{
                case r:RangePosition =>
                  val dollar = "$"
                  sReporter.report(Info, s"actualTextExpr is a StringLiteral. Here is its range: {(${r.lineFrom}, ${r.colFrom})-(${r.lineTo}, ${r.colTo})}")
              }
              actualTextExpr.getPos.file.getName match {
                //               Is there a better way to check if the constant comes from the library?
                case "WebBuilder.scala" =>
                  //                This StringLiteral comes from the code of WebBuilder.scala and not from the user's source code,
                  //                 so it is not to be modified by a user StringModification and should therefore be seen as a constant String by StringSolver
                  (List(Left(string)), assignmentMap, posToId)
                case _ =>
                  //                This StringLiteral comes from the user's source code, so it can be modified by a user StringModification.
                  //                 We generate an identifier for it so that StringSolver is able to assign a new value to this StringLiteral.
                  if (actualTextExpr.getPos.line == -1) throw new Exception("Line is -1 on " + actualTextExpr)
                  val identifier = posToId.getOrElse(actualTextExpr.getPos, Common.FreshIdentifier("l:" + actualTextExpr.getPos.line + ",c:" + actualTextExpr.getPos.col, alwaysShowUniqueID = true).copiedFrom(actualTextExpr))
                  sReporter.report(Info, "Creating identifier " + identifier + " -> file " + actualTextExpr.getPos.file.getName + " \"" + string + "\"")
                  (List(Right(identifier)), assignmentMap + (identifier -> string), posToId + (actualTextExpr.getPos -> identifier))
              }
            case StringConcat(tExpr1, tExpr2) =>
              recFunction(tExpr1, sReporter, assignmentMap, posToId) match {
                case (strForm1, assignMap1, posToID1) =>
                  recFunction(tExpr2, sReporter, assignMap1, posToID1) match {
                    case (strForm2, assignMap2, posToID2) =>
                      (strForm1 ++ strForm2, assignMap2, posToID2)
                  }
              }
            case aTE@_ =>
              val errorMEssage = "Match Error: the following expr is neither a StringLitteral nor a StringConcat: \n\t"+actualTextExpr
              sReporter.report(Error, errorMEssage)
              failure(errorMEssage)
          }
        }
        recFunction(textExpr, sReporter, Map(), Map())
      }

      //      The first argument of the protoNewClarificationSession will be used as the first argument of the definitive NewClarificationSession (the one that will be registered in Memory)
      val (solutions: Stream[Map[Identifier, String]], protoNewClarificationSession: ClarificationSession, equations: List[Equation]) = {
        //        Assumes the modified webElement is a TextElement
        def generateStringEquationAndAssignment(teID: Int, sourceMap: SourceMap, serverReporter: ServerReporter): (Equation, Map[Identifier, String]) = {
          val sReporter = serverReporter.startFunction("generateStringEquationAndAssignment on TextElementID: "+teID)
          val (webElement, unevaluatedExprOfWebElement) = getWebElemAndUnevalExprOfWebElemFromSourceMap(teID, sourceMap, sReporter)
          //          Checking that the webElement is a TextElement
          webElement match {
            case TextElement(text)=>
              val unevaluatedExprOfTextElementTExt = unevaluatedExprOfWebElement match {
                case CaseClass(CaseClassType(_, _), argSeq) => argSeq.head
              }
              //              textExprToStringFormAndAssignmentMap(unevaluatedExprOfWebElement, sReporter) match {
              textExprToStringFormAndAssignmentMap(unevaluatedExprOfTextElementTExt, sReporter) match {
                case (stringForm, mapIdToString, _) => ((stringForm, text), mapIdToString)
              }
            case _ => failure(s"The WebElement of id $teID is not a TextElement")
          }
        }
        def printAndMergeDuplicateIdentifiersAndSolveEquationsAndAssignmentThenReturnResult(
                                                                                             equations: List[Equation],
                                                                                             assignment: Map[Identifier, String],
                                                                                             protoNewClarificationSession: ClarificationSession,
                                                                                             sReporter: ServerReporter
                                                                                           ) = {
          /**
            * Assumes all the identifier present in its arguments come from the same File.
            * Make sure that the same instance of the Identifier class is used consistently through the equations and assignments
            *
            *(So that there is no longer 2 different instances of the Identifier class used at different places while they have the same line and column number)
            *
            * @param equations
            * @param assignment
            * @return
            */
          def mergeDuplicatedIdentifiers(equations: List[Equation], assignment: Map[Identifier, String]): (List[Equation], Map[Identifier, String]) = {
            //            val i : Int = equations.head._1.head match {case Right(id)=> id.getPos.file}
            case class IdentifierLineAndCol(line: Int, column: Int)
            val finalIdentifiers = scala.collection.mutable.Map[IdentifierLineAndCol, Identifier]()
            /** Building the finalIdentifier map **/
            equations.foreach((equation) =>{
              equation. _1.collect{
                case Right(identifier) =>
                  finalIdentifiers += (IdentifierLineAndCol(identifier.getPos.line, identifier.getPos.col) -> identifier)
              }
            })
            assignment.keys.foreach((identifier)=>{
              finalIdentifiers += (IdentifierLineAndCol(identifier.getPos.line, identifier.getPos.col) -> identifier)
            })

            /** Rebuilding the equations and assignments, so that the new ones contains the exact same identifiers instead of different ones with the same line and columns values **/
            val rebuiltEquations = equations.map((equation) => {
              val leftHandSide = equation._1.map{
                case Left(string) => Left(string)
                case Right(identifier) => Right(finalIdentifiers(IdentifierLineAndCol(identifier.getPos.line, identifier.getPos.col)))
              }
              val rightHandSide = equation._2
              (leftHandSide, rightHandSide)
            })
            val rebuiltAssignment = assignment.foldLeft(Map[Identifier, String]()){
              case (sketchyAssignment, (identifier, string)) =>
                sketchyAssignment + (finalIdentifiers(IdentifierLineAndCol(identifier.getPos.line, identifier.getPos.col)) -> string)
            }

            (rebuiltEquations, rebuiltAssignment)
          }
          val (equationsWithMergedIdentifiers, assignmentWithMergedIdentifiers) = mergeDuplicatedIdentifiers(equations, assignment)

          sReporter.report(Info, "Printing the equations:")
          val ssReporter = sReporter.addTab
          equationsWithMergedIdentifiers.foreach(equation => ssReporter.report(Info, equation.toString))
          sReporter.report(Info, "Printing the Assignment:")
          ssReporter.report(Info, assignmentWithMergedIdentifiers.toString)

          val solutions = StringSolver.solveMinChange(equationsWithMergedIdentifiers, assignmentWithMergedIdentifiers)
          var counter = 0
          solutions.take(maxNumberOfConsideredSolutions). foreach({ sol =>
            counter += 1
            sReporter.report(Info, "Checking solution "+ counter+":")
            val ssReporter = sReporter.addTab
            ssReporter.report(Info, sol.toString)
            StringSolver.errorcheck(equationsWithMergedIdentifiers, assignmentWithMergedIdentifiers ++ sol) match {
              case None => ssReporter.report(Info, "errorCheck sent None -> solution "+ counter +" is correct")
              case s@_ => ssReporter.report(Info ,"errorCheck didn't send None (" + s + ") -> solution " + counter + " is NOT correct (maybe a problem in StringSolver.scala)")
            }
          })
          (
            StringSolver.solveMinChange(equationsWithMergedIdentifiers, assignmentWithMergedIdentifiers),
            protoNewClarificationSession,
            equationsWithMergedIdentifiers
            )
        }
        if (Memory.clarificationSessionOption.isEmpty || !Memory.clarificationSessionOption.get.idsOfInvolvedTextElements.contains(weID)) {
          //          Either there was no clarification in progress, or the modified TextElement has nothing to do with the clarification in progress.
          //          In this case, we only use the equation created from this string modification.
          if (Memory.clarificationSessionOption.isEmpty) {
            sReporter.report(Info, "There was no clarification in progress. Starting a new one.")
          }
          else {
            //            In this case, !Memory.clarificationSessionOption.get.idsOfInvolvedTextElements.contains(weID) is necessarily true
            sReporter.report(Info, "There was a clarification in progress, but the latest String Modification is on a TextElement that has nothing to do with it. Starting a new clarification")
          }

          //          "protoEquation" has the correct concatenating structure (left hand side of the equation), but the total string
          //          result (right hand side) wrong, because it is built from the webPage stored in the sourceMap (hence the PREVIOUS webPage)
          //          As such, it must be modified to have the correct right hand term.
          val (protoEquation, assignment) = generateStringEquationAndAssignment(weID, sourceMap, sReporter)
          val equation = (protoEquation._1, newVal)
          printAndMergeDuplicateIdentifiersAndSolveEquationsAndAssignmentThenReturnResult(List(equation), assignment, ClarificationSession(List(weID), List()), sReporter)
        }
        else {
          //          There is a clarification in progress, and the modified TextElement is involved in it
          sReporter.report(Info, "There was a clarification in progress, and the newly modified TextElement is involved in it.")
          val clarificationSession = Memory.clarificationSessionOption.get
          val textElementIDsToEquationsInPreviousWebPageMap_and_previousAssignments: (Map[Int, Equation], List[Assignment]) =
            Memory.clarificationSessionOption
              .getOrElse(ClarificationSession(List(), List()))
              .textElementIdsForEquations
              .foldLeft((Map[Int, Equation](), List[Assignment]())) {
                case ((underConstructionMap, underConstructionAssignmentsList), textElementID) =>
                  val newEquationAndAssignment = generateStringEquationAndAssignment(textElementID, sourceMap, sReporter)
                  (underConstructionMap + (textElementID -> newEquationAndAssignment._1), underConstructionAssignmentsList :+ newEquationAndAssignment._2)
              }
          val textElementIDsToEquationsInPreviousWebPageMap: Map[Int, Equation] = textElementIDsToEquationsInPreviousWebPageMap_and_previousAssignments._1
          val previousAssignments: List[Assignment] = textElementIDsToEquationsInPreviousWebPageMap_and_previousAssignments._2
          val previousAssignmentsFusion: Map[Identifier, String] = previousAssignments.foldLeft(Map[Identifier, String]())(
            { (accumulator: Map[Identifier, String], assignment: Map[Identifier, String]) => accumulator ++ assignment })

          if (clarificationSession.textElementIdsForEquations.contains(weID)) {
            //            The modified TextElement has already been clarified, we update its equation in the problem.
            sReporter.report(Info, "The modified TextElement has already been clarified, we update its equation in the clarification problem.")
            val updatedEquation = {
              val previousEquation = textElementIDsToEquationsInPreviousWebPageMap(weID)
              val ssReporter = sReporter.addTab
              sReporter.report(Info,"Previous equation to be updated:")
              ssReporter.report(Info, previousEquation.toString)
              val updatedEquation = (previousEquation._1, newVal)
              sReporter.report(Info,"Updated equation:")
              ssReporter.report(Info, updatedEquation.toString)
              updatedEquation
            }
            val newTextElementIDToEquationMap = textElementIDsToEquationsInPreviousWebPageMap + (weID -> updatedEquation)
            printAndMergeDuplicateIdentifiersAndSolveEquationsAndAssignmentThenReturnResult(
              newTextElementIDToEquationMap.values.toList,
              previousAssignmentsFusion, clarificationSession, sReporter
            )
          }
          else {
            //            The modified TextElement has not already been clarified.
            //            We generate its equation, then if adding it to the problem makes the latter unsolvable,
            //              we start a new clarification with only this equation. Else, we add it to the problem.
            sReporter.report(Info, "The modified TextElement has not already been clarified. We generate its equation and" +
              " add it to the clarification problem provided it does not make it unsolvable. If it does, we launch a new clarification problem with only this equation")

            //            "protoEquation" has the concatenating structure right (left side of the equation), but the total string
            //            result (right side) wrong, because it is built from the webPage stored in the sourceMap (hence the PREVIOUS webPage)
            //            As such, it must be modified to have the right right hand term.
            val (newProtoEquation, newAssignment) = generateStringEquationAndAssignment(weID, sourceMap, sReporter)
            val newEquation = (newProtoEquation._1, newVal)
            val finalEquations = textElementIDsToEquationsInPreviousWebPageMap.values.toList :+ newEquation
            val finalAssignment = previousAssignmentsFusion ++ newAssignment
            printAndMergeDuplicateIdentifiersAndSolveEquationsAndAssignmentThenReturnResult(
              finalEquations,
              finalAssignment,
              ClarificationSession(clarificationSession.textElementIdsForEquations :+ weID, List()),
              sReporter
            )
          }
        }

      }

      implicit val context = LeonContext.empty
      val fileInterface = new FileInterface(context.reporter)

      /**
        * Modify the original sourceCode by applying the changes from the solution
        *
        * @param sourceCode
        * @param solution
        * @return
        */
      def applySolutionToSourceCode(sourceCode: String, solution: StringSolver.Assignment, serverReporter: ServerReporter): (String, List[StringPositionInSourceCode]) = {
        val sReporter = serverReporter.startFunction("applySolutionToSourceCode")
        sReporter.report(Info,
          s"""Arguments:
              |  base source code: $sourceCode
              |  solution to apply: $solution
           """.stripMargin)
        val changedElements = ListBuffer[StringPositionInSourceCode]()
        val res1 = solution.toList
          .sortBy({ case (identifier, str) => identifier.getPos })
          .reverse
          .foldLeft(sourceCode)({
            case (sCode, (identifier, string)) =>
              changedElements += (identifier.getPos match {
                case RangePosition(lineFrom, colFrom, pointFrom, lineTo, colTo, pointTo, file) => StringPositionInSourceCode(lineFrom, colFrom, lineTo, colTo)
                case _ => StringPositionInSourceCode(0, 0, 0, 0)
              })
              identifier.getPos match {
                case r: RangePosition =>
                  sReporter.report(Info, s"""Replacing from (${r.lineFrom}, ${r.colFrom}) to (${r.lineTo}, ${r.colTo}) with "$string" (I guess)""")
              }
              fileInterface.substitute(sCode, identifier, StringLiteral(string))
          }
          )
        sReporter.report(Info,
          s"""Resulting Source Code:
              | $res1
          """.stripMargin)
        (res1, changedElements.toList)
      }
//
//      /**
//        * Modify the original program by applying the changes from the solution
//        *
//        * @param originalProgram
//        * @param solution
//        * @return
//        */
//      def applySolutionToProgram(originalProgram: Program, solution: StringSolver.Assignment): Program = {
//        import leon.purescala.DefOps
//        //    Warning: This do not replace top-level vals
//        val newProgram = DefOps.replaceFunDefs(originalProgram)({ fd =>
//          if (ExprOps.exists(e => solution.exists({ case (id, str) => id.getPos == e.getPos }))(fd.fullBody)) {
//            val newFD = fd.duplicate()
//            newFD.fullBody = ExprOps.preMap(e => solution.find({ case (id, str) => id.getPos == e.getPos }) match {
//              case Some((id, str)) =>
//                //                The ".copiedFrom(id)" is here so that the new StringLiteral has (in particular) the same value for his
//                //                  position than the one it replaces (id)
//                val newIdentifier = StringLiteral(str).copiedFrom(id)
//                Some(newIdentifier)
//              case None => None
//            })(fd.fullBody)
//            Some(newFD)
//          }
//          else None
//        })._1
//        newProgram
//      }

      def applySolution(sourceCode: String, program: Program, solution: StringSolver.Assignment): (String, Program, Option[FunDef], List[StringPositionInSourceCode]) = {
        println("Transforming a solution...")
        solution.toList
          .sortBy({case (identifier, str) => identifier.getPos})
          .reverse
          .foldLeft((sourceCode, program, ProgramEvaluator.functionToEvaluate, List[StringPositionInSourceCode]()))(
            {case ((sCode, prog, optFunDef, changedElements), (identifier, string)) =>
              val newChangedElements = changedElements :+ {identifier.getPos match {
                case RangePosition(lineFrom, colFrom, pointFrom, lineTo, colTo, pointTo, file) => StringPositionInSourceCode(lineFrom, colFrom, lineTo, colTo)
                case _ => StringPositionInSourceCode(0, 0, 0, 0)
              }}
              val replacement = StringLiteral(string).copiedFrom(identifier)
              val transformer = DefOps.funDefReplacer(fd => {
                if(ExprOps.exists{ e => e.getPos == identifier.getPos }(fd.fullBody)) {
                  val newFd = fd.duplicate()
                  newFd.fullBody = ExprOps.postMap{
                    case e if e.getPos == identifier.getPos =>
                      Some(replacement)
                    case _ => None
                  }(fd.fullBody)
                  Some(newFd)
                } else None
              })
              val newProg = DefOps.transformProgram(transformer, prog)
              val optNewFun = optFunDef.map(transformer.transform)
              (fileInterface.substitute(sCode, identifier, replacement),
                newProg,
                optNewFun,
                newChangedElements
              )
            }
          )
      }

      def buildListOfStringPositionsForModifiedIdentifier(solution: StringSolver.Assignment): List[StringPositionInSourceCode] = {
        solution.map({ case (identifier, str) =>
          identifier.getPos match {
            case RangePosition(lineFrom, colFrom, pointFrom, lineTo, colTo, pointTo, file) => StringPositionInSourceCode(lineFrom, colFrom, lineTo, colTo)
            case _ => StringPositionInSourceCode(0, 0, 0, 0)
          }
        }).toList
      }

      sReporter.report(Info, "First " + maxNumberOfConsideredSolutions + " StringSolver solutions:")
      val ssReporter = sReporter.addTab
      solutions.take(maxNumberOfConsideredSolutions).foreach(assignment => ssReporter.report(Info, assignment.toString))

      val originalProgram = sourceMap.program

      case class ClarificationQuestionMakingFailure(msg: String) extends java.lang.Exception(msg, null)
      def clarificationFailure(failureMessage: String) = {
        println(failureMessage)
        throw new StringModificationProcessingException(s"Failure when attempting to create a clarification question: ${failureMessage}")
      }

      def leonListToList[T](leonList: leon.collection.List[T]): List[T] = {
        val listBuffer = leonList.foldLeft(ListBuffer[T]())((list, elem) => list += elem)
        listBuffer.toList
      }

      case class TextElementWithID(textElement: TextElement, id: Int)

      /**
        * Extract the list of the TextElements of a webPageWithIDedWebElements, along with their ids
        *
        * @param webPageWithIDedWebElements
        * @return
        */
      def extractTextElementsWithIDFromWebPage(
                                                webPageWithIDedWebElements: WebPageWithIDedWebElements,
                                                serverReporter: ServerReporter
                                              ): List[TextElementWithID] = {
        val sReporter = serverReporter.startFunction("extractTextElementsWithIDFromWebPage")
        def textElementExtractor(webElementWithID: WebElementWithID): List[TextElementWithID] = {
          //          sReporter.report(Info, "Extracting from: "+webElementWithID)
          webElementWithID match {
            case WebElementWithID(webElem, id) =>
              webElem match {
                case Element(_, sons, _, _) =>
                  //                  sReporter.report(Info, "Element found: "+webElem)
                  leonListToList[WebElement](sons).map(we => we.asInstanceOf[WebElementWithID]).flatMap(textElementExtractor)
                case t@TextElement(_) =>
                  //                  sReporter.report(Info, "TextElement found: "+webElem)
                  List(TextElementWithID(t, id))
                case WebElementWithID(_, _) => failure("There was a WebElementWithID packed in another WebElementWithID, which should never happen")
              }
          }
        }
        textElementExtractor(webPageWithIDedWebElements.main)
      }

      val _result: StringModificationSubmissionResult = if (solutions.isEmpty) {
        //      No solution
        sReporter.report(Error, "No solution found by StringSolver. Cleaning the clarification session.")
        Memory.clarificationSessionOption = None
        failure("No solutions found by StringSolver")
        //        StringModificationSubmissionResult(PotentialWebPagesList(None, List()), "No solutions found by StringModificationProcessor")
      }
      else {
        //      At least one solution

        case class RawSolution(
                                sourceCode: String,
                                idedWebPage: WebPageWithIDedWebElements,
                                positionsOfModificationsInSourceCode: List[StringPositionInSourceCode],
                                sourceMapProducer: () => Option[SourceMap]
                              )

        sReporter.report(Info, "String Solver found at least one solution")
        var leonContextOfFirstSolution: Option[LeonContext] = None
        val rawSolutions: List[RawSolution] =
          solutions.take(maxNumberOfConsideredSolutions).toList.flatMap(
            solution => {
              val (newSourceCode, newProgram, newFuntionToExecuteOption, changedElements) = applySolution(sourceCode, originalProgram, solution)
//              val newFunctionToExecute  = newFuntionToExecuteOption match {
//                case Some(f) => f
//                case None => failure("The \"applySolution\" function did not yield the new function to evaluate")
//              }
              ProgramEvaluator.evaluateAndConvertResult(
                newProgram,
                newSourceCode,
                newFuntionToExecuteOption,
                serverReporter
              ) match {
                case (None, evaluationLog) =>
                  //            Memory.setSourceMap(newSourceId, () => None)(null)
                  serverReporter.report(Error,
                    s"""
                       |ProgramEvaluator did not manage to evaluate and unexpr the result of the leon program.
                       | Here is the evaluation log: $evaluationLog
                    """.stripMargin)
                  None
                case (Some((idedWebPage, sourceMapProducer, ctx)), evaluationLog) =>
                  leonContextOfFirstSolution = Some(ctx)
                  Some(RawSolution(newSourceCode, idedWebPage, changedElements, sourceMapProducer))
              }
            }
          )
        sReporter.report(Info, "Number of considered solutions: "+rawSolutions.length)
        if(rawSolutions.nonEmpty ){
          sReporter.report(Info, "Raw solution 1: ")
          val ssReporter = sReporter.addTab
          ssReporter.report(Info, "idedWebPage: "+rawSolutions.head.idedWebPage)
        }
        /**
          * Takes as input a list of raw solutions, and an index in this list.
          * Takes the designated raw solution and reformat it (removing elements from the tuple and changing their order)
          *
          * @param rawSolutionsList a list of raw solutions, containing the one to be formatted
          * @param indexOfSolutionToFormat an index in the rawSolutionsList, indicating the rawSolution to be formatted
          * @return The formatted solution
          */
        def formatSolution(rawSolutionsList: List[(WebPageWithIDedWebElements, () => Option[SourceMap], String, List[StringPositionInSourceCode])],
                           indexOfSolutionToFormat: Int
                          ): (String, WebPageWithIDedWebElements, List[StringPositionInSourceCode]) = {
          rawSolutionsList(indexOfSolutionToFormat) match {
            case (webPageWithIDWebElem, sourceMapProducer, sourceCode_, changedElements) =>
              (sourceCode_, webPageWithIDWebElem, changedElements)
          }
        }

        def turnIntoShippableSolution(rawSolution: RawSolution, textContentOfClarifiedWebElementOption: Option[String]): ShippableClarificationSolution = {
          rawSolution match {
            case RawSolution(sourceCode_, idedWebPage, positionsOfModificationsInSourceCode, sourceMapProducer) =>
              ShippableClarificationSolution(sourceCode_, idedWebPage, positionsOfModificationsInSourceCode, textContentOfClarifiedWebElementOption)
          }
        }

        if (solutions.tail.isEmpty) {
          //      Exactly one solution
          sReporter.report(Info, "Proposing exactly one solution. Cleaning the clarification session.")
          Memory.clarificationSessionOption = None
          StringModificationSubmissionResult(
            PotentialWebPagesList(
              newSourceCodeID = Some(newSourceId),
              solutionList = List(turnIntoShippableSolution(rawSolutions.head, None)),
              idOfClarifiedWebElementOption = None
            ),
            "Log: Only one solution"
          )
        }
        else {
          //      More than one solution
          /**
            * //            * Traverse the input list, removing its elements whose first elements (TextElementWithID) is a duplicate of
            * //            *  a previously seen first element (comparing their texts).
            * //            * Also returns the list of the indexes in the input list of its elements (List[TextElementWithID]) that were kept
            * //            *
            * Find each index for which the TextElements of the lists of textElementListList at this index do not all have the same texts.
            * If there are no such indexes, keep only the first list of TextElement. (Return (List(0), textElementListList.head, None))
            * Else let's call idx the first of these indexes
            * Traverse the list of the TextElements present at index idx in the lists of TextElements.
            * When encountering a TextElement that has the same text as a previously encountered TextElement, remove
            * the list of TextElement the former comes from.
            * Returns a triplet:
            * - list of the indexes in textElementListList of the lists of TextElements that were not removed
            * - textElementListList without the removed lists of TextElements
            *   - idx (see description above)
            *
            * @param textElementListList List of the Lists of the TextElements of some structurally identical webpages (with ID)
            * @return see description above
            */
          def horizontalPruner(
                                textElementListList: List[List[TextElementWithID]],
                                serverReporter: ServerReporter
                              ): (List[Int], List[List[TextElementWithID]], Option[Int]) = {
            val sReporter = serverReporter.startFunction("horizontalPruner")
            if(textElementListList.isEmpty) {
              sReporter.report(Info, "horizontal pruner was given an empty list")
              return (List(), List(), None)
            }

            {
              sReporter.report(Info, """Printing the lists of TextElements extracted from the considered solutions webpages: """)
              val ssReporter = sReporter.addTab
              textElementListList.foreach(textElementList => {
                val ssReporter = sReporter.addTab
                ssReporter.report(Info, "Length: " + textElementList.length)
                textElementList.foreach(textElementWithID => {
                  ssReporter.report(Info, "- " + textElementWithID.textElement.text)
                })
              }
              )
            }
            //            The list of the indexes such that not all TextElements at these indexes in the lists of TextElements have the same texts
            val indexOfChangesList = ListBuffer[Int]()
            var examinedTextElementListList = textElementListList
            for(index <- textElementListList.head.indices) { sReporter.report( Info, "Examining TextElements at index "+index+" to find text differences")
              val firstTextElementsList = examinedTextElementListList.map(textElementList => textElementList.head.textElement)
              val referenceText = firstTextElementsList.head.text
              //                Removing the first elements of the lists in examinedTextElementListList
              examinedTextElementListList = examinedTextElementListList.map(textElementList => textElementList.tail)
              if (firstTextElementsList.exists(textElement => textElement.text != referenceText)) {
                sReporter.report(Info,
                  s"""Found a text difference between the TextElements at index $index.
                      |  Here is the text of the first TextElement at this index:
                      |    $referenceText
                   """.stripMargin)
                indexOfChangesList += index
              }
              else {
                sReporter.report(Info, "No text difference found between the TextElements at index " + index)
              }
            }
            //            Looking at the different texts for the first TextElements that do not have the same text in all the lists
            //             of TextElements and pruning the list of lists of TextElement from the lists of TextElements that gives
            //             this first TextElements the same text than another list of TextElement
            if (indexOfChangesList.isEmpty) {
              //              No differences detected between the proposed webpages
              sReporter.report(Info, "The considered solution webpages were not different from each others. Pruning all but the first one.")
              (List(0), List(textElementListList.head), None)
            }
            else {
              sReporter.report(Info,"At least one difference was found between the considered solution webpages")
              val indexOfFirstChange : Int = indexOfChangesList.head
              return textElementListList.foldLeft((0, List[String](), (List[Int](), List[List[TextElementWithID]](), Some(indexOfFirstChange))))(
                (accumulator, textElementList) => {
                  accumulator match {
                    case (currentIndex, previouslySeenTexts, (resultIndexList, resultListOfListOfTextElements, idxOfFirstChange)) =>
                      val text = textElementList(indexOfFirstChange).textElement.text
                      if (previouslySeenTexts.contains(text)) { //                      Remove this list of TextElements
                        (currentIndex+1, previouslySeenTexts, (resultIndexList, resultListOfListOfTextElements, idxOfFirstChange))
                      }
                      else {
                        //                      Keep this list of TextElements, and store the text
                        (currentIndex+1 , previouslySeenTexts:+ text , (resultIndexList:+currentIndex, resultListOfListOfTextElements:+textElementList, idxOfFirstChange))
                      }
                  }
                }
              )._3
            }
          }

          val extractedTextElementsWithID : List[List[ TextElementWithID]] = rawSolutions.map(
            (rawSolution) => extractTextElementsWithIDFromWebPage(rawSolution.idedWebPage, sReporter)
          )
          val (
            proposedSolutionIndexes: List[Int],
            proposedSolutionsTextElements: List[List[TextElementWithID]],
            indexOfFirstTextDifferenceInTextElementsListsOption: Option[Int]
            ) = {
            val all = horizontalPruner(
              rawSolutions.map(
                (rawSolution) => extractTextElementsWithIDFromWebPage(rawSolution.idedWebPage, sReporter)
              ),
              sReporter)
            val prunned = List(all._1, all._2).map((list) => list.take(maxNumberOfProposedSolutions))
            (prunned(0), prunned(1), all._3)
          }

          val proposedSolutionsList : List[RawSolution] = proposedSolutionIndexes.map(index => rawSolutions(index))
          val intermediateCouple : (Option[Int], List[Option[String]])=
            indexOfFirstTextDifferenceInTextElementsListsOption match {
              case None => (None, proposedSolutionsTextElements.map(_ => None))
              case Some(index) => {
                val firstReturnValue = Some(proposedSolutionsTextElements.head(index).id)
                val secondReturnValue = proposedSolutionsTextElements.map(
                  (textElementsOfASolution) => {
                    Some(textElementsOfASolution(index).textElement.text)
                  }
                )
                (firstReturnValue, secondReturnValue)
              }
            }
          val idOfFirstTextElementToDifferBetweenTheSolutions : Option[Int] =  intermediateCouple._1
          val proposedSolutionsTextOfClarifiedWebElementOption: List[Option[String]] = intermediateCouple._2
          val shippableSolutionsList = proposedSolutionsList.zip(proposedSolutionsTextOfClarifiedWebElementOption).map {
            case (RawSolution(sourceCode_, idedWebPage, positionsOfModificationsInSourceCode, _), textContentOfClarifiedWebElementOption) =>
              ShippableClarificationSolution(sourceCode_, idedWebPage, positionsOfModificationsInSourceCode, textContentOfClarifiedWebElementOption)
          }
          sReporter.report ( Info, "Proposing "+ proposedSolutionsList.length+" solutions")

          //      TODO:  The case where leonContextOfFirstSolution is None could maybe be handled nicer (currently, it throws an exception)
          Memory.setAutoSourceMap(
            newSourceId,
            proposedSolutionsList.head.sourceMapProducer
          )(leonContextOfFirstSolution.get)

          val idsOfInvolvedTextElements : List[Int] = {
//            val allIdentifiersInEquations: List[Identifier] = solutions.head.keys.toList
            val allIdentifiersInEquations: List[Identifier] = equations.foldLeft(List[Identifier]()){
              case (identifiersAccumulator, equation) => identifiersAccumulator ++ equation._1.flatMap({
                case Left(string) => None
                case Right(identifier) => Some(identifier)
              })
            }
//            println("allIdentifiersInEquations= "+allIdentifiersInEquations)
            val posOfAllIdentifiersInEquations: List[Position] = allIdentifiersInEquations.map((identifier) => identifier.getPos)
//            println("posOfAllIdentifiersInEquations= "+posOfAllIdentifiersInEquations)
            val allIdsOfWebElementsInSourceMap: List[Int] = sourceMap.keys.toList
//            println("allIdsOfWebElementsInSourceMap= "+allIdsOfWebElementsInSourceMap)
            val allWebElementsUnevaluatedExprInOriginalWebPage : List[Expr] = allIdsOfWebElementsInSourceMap.map{ id => sourceMap.webElementIDToExpr(id) .optionValue.get}
//            println("allWebElementsUnevaluatedExprInOriginalWebPage= "+allWebElementsUnevaluatedExprInOriginalWebPage)
            val allWebElementsInOriginalWebPage: List[WebElement] = allIdsOfWebElementsInSourceMap.map{ id => sourceMap.webElementIDToWebElement(id). optionValue.get}
            //            The identifiers in allIdentifiersInEquations should come from the same program than the unevaluated exprs
            //             in allWebElementsUnevaluatedExprInOriginalWebPage, to ensure that the equality test works properly.
            //            Filtering the (id, unevalExpr) couples by only keeping those that corresponds to TextElements
//            println("allWebElementsInOriginalWebPage= "+allWebElementsInOriginalWebPage)
            val webElemIDsAndUnevaluatedExprOfAllTextElementsInOriginalWebPage: List[(Int, Expr)] =
              allIdsOfWebElementsInSourceMap.zip(allWebElementsUnevaluatedExprInOriginalWebPage).zip( allWebElementsInOriginalWebPage).collect{
                case (idAndUnevalExpr: (Int, Expr), webElement: TextElement) => idAndUnevalExpr
              }
//            println("webElemIDsAndUnevaluatedExprOfAllTextElementsInOriginalWebPage= "+webElemIDsAndUnevaluatedExprOfAllTextElementsInOriginalWebPage)
            val returnValue = webElemIDsAndUnevaluatedExprOfAllTextElementsInOriginalWebPage.collect {
              case (webElemId: Int, unevalTextElemExpr: Expr) if ExprOps.exists {
                case s: StringLiteral => posOfAllIdentifiersInEquations.contains(s.getPos)
                case _ => false
              } (unevalTextElemExpr) => webElemId
            }
//            println("idsOfInvolvedTextElements= "+returnValue)
            returnValue
          }

          val finalNewClarificationSession = ClarificationSession(
            protoNewClarificationSession.textElementIdsForEquations,
            idsOfInvolvedTextElements
          )
          Memory.clarificationSessionOption = Some(finalNewClarificationSession)
          StringModificationSubmissionResult(
            PotentialWebPagesList(
              newSourceCodeID = Some(newSourceId),
              shippableSolutionsList,
              idOfFirstTextElementToDifferBetweenTheSolutions
            ),
            "Log: multiple solutions"
          )
        }
      }
      _result
    }
    catch {
      case StringModificationProcessingException(msg) =>
        throw new Exception(msg)
    }
    result
  }
}

/**
  * To be stored and reused along with the client-server dialogue on the resolving of a string modification ambiguity
  *
  * @param textElementIdsForEquations Stores the ids of the TextElement from which an equation should be generated
  * @param idsOfInvolvedTextElements: The ids of all the TextElements whose texts are produced thanks to at least one
  *                                string element that appears in one of the equation
  */
case class ClarificationSession(textElementIdsForEquations: List[Int], idsOfInvolvedTextElements: List[Int])