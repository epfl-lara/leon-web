package leon.web.websitebuilder.webPageExprConversion

import leon.collection.Cons
import leon.purescala.Definitions.{CaseClassDef, Program}
import leon.purescala.Expressions.{CaseClass, Expr, Literal}
import leon.purescala.Types.CaseClassType
import leon.web.websitebuilder.logging.serverReporter.{Error, Info, ServerReporter}
import leon.webDSL.webDescription.{Element, StyleSheet, TextElement, WebElement, WebElementWithID, WebPage, WebPageWithIDedWebElements}

import scala.collection.mutable.ListBuffer

/**
  * Created by dupriez on 28/06/16.
  */
object WebPageExprToIdedWebPageAndSourceMap {
  private case class WebPageExprToIdedWebPageAndSourceMapException(msg: String) extends Exception

  def unexprAndIDWebPageAndStartBuildingSourceMap(webPageConcreteExpr: Expr, program: Program, sourceCode: String, serverReporter: ServerReporter): (WebPageWithIDedWebElements, Expr => SourceMap) = {
    val sReporter = serverReporter.startProcess("Unexpring WebPage, giving ids to its WebElements, and starting building sourceMap")
    convertWebPageExprToClientWebPageAndSourceMap(webPageConcreteExpr, program, sourceCode, sReporter) match {
      case Some((webPageWithIdedWebElements, unevalExprToSourceMapFunction)) =>
        (webPageWithIdedWebElements, unevalExprToSourceMapFunction)
      case None =>
        val errorMessage = "Unable to complete: \"Unexpring WebPage, giving ids to its WebElements, and starting building sourceMap\""
        sReporter.report(Error, errorMessage)
        throw WebPageExprToIdedWebPageAndSourceMapException(errorMessage)
    }
  }

//  TODO: Make this cleaner
  // The second element takes the result of the abstract Evaluator and updates the source map.
  private def convertWebPageExprToClientWebPageAndSourceMap(webPageEvaluatedExpr: Expr, program: Program, sourceCode: String, serverReporter: ServerReporter): Option[(WebPageWithIDedWebElements, Expr => SourceMap)] = {
    case class ConversionException(msg: String) extends Exception

    val sReporter = serverReporter.startFunction("Converting the WebPage Expr into a WebPage, and building the sourceMap")
    sReporter.report(Info, "webPage expr to be converted: "+ "DISABLED (to re-enable it, look for \"#VERBOSITY\" in ProgramEvaluator.scala)")
    //    #VERBOSITY
    //    sReporter.report(Info, "webPage expr to be converted: "+ webPageEvaluatedExpr)

    val result: Either[(WebPageWithIDedWebElements, Expr => SourceMap), String] = try {
      /** Looking up the case classes of webDSL_Leon**/
      def lookupCaseClass(program: Program)(caseClassFullName: String): CaseClassDef = {
        program.lookupCaseClass(caseClassFullName) match {
          case Some(classDef) => classDef
          case None => {
            val msg = "Conversion failed, lookupCaseClass(\"" + caseClassFullName + "\") gave no result"
            sReporter.report(Error, msg)
            throw ConversionException(msg)
          }
        }
      }

      // Maps CaseClassDefs to an associated reflect constructor.
      val constructorMap = WebDescriptionClassesRegister.fullNameToConstructorMap.map({case (fullName, constructor) => (lookupCaseClass(program)(fullName), constructor)})

      def unExpr(sReporter: ServerReporter)(e: Expr): Any = {
        //sReporter.report(Info, "Unexpring " + e)
        val actualExpr = TupleSelectAndCaseClassSelectRemover.removeTopLevelTupleSelectsAndCaseClassSelects(e)
        actualExpr match {
          case CaseClass(CaseClassType(caseClassDef, targs), args) => {
            constructorMap.get(caseClassDef) match {
              case Some((constructor, isWebElement)) =>
                val unexpredThing = constructor(args.map (unExpr(sReporter)): _*)
                //                if (unexpredThing.isInstanceOf[WebElement]) {
                //                  protoSourceMap.addMapping(new WebElementWrap(unexpredThing.asInstanceOf[WebElement]), e)
                unexpredThing
              case None =>
                val msg = s"""
                             |Looked for ${caseClassDef.toString} in the constructorMap, but did not find anything. Throwing exception.
                             |   Maybe ${caseClassDef.toString} is not registered in server/app/programEvaluator/WebDescriptionClassesRegister.
                  """.stripMargin
                throw ConversionException(msg)
            }
          }
          case l: Literal[_] => l.value
          //            unapply magic
          //          case TupleSelectOrCaseClassSelect(actualExpr) => unExpr(sReporter)(actualExpr)
          case _ =>
            //            unExpr(sReporter)(stringModification.StringModificationProcessor.simplifyCaseSelect(e))
            sReporter.report(Info, "Unexpr default case, something is probably wrong")
        }
      }

      def buildSourceMapAndGiveIDsToWebElements(webPage: WebPage, sourceCode: String, program: Program, serverReporter: ServerReporter): (WebPageWithIDedWebElements, Expr => SourceMap) = {
        val sReporter = serverReporter.startFunction("buildSourceMapAndGiveIDsToWebElements")
        val WebPage(bootstrapWebElement: WebElement, bootstrapcss: StyleSheet) = webPage
        val cb = new ConversionBuilder(program, sReporter)

        def leonListToList[T](leonList: leon.collection.List[T]): List[T] = {
          val listBuffer = leonList.foldLeft(ListBuffer[T]())((list, elem)=>list += elem)
          listBuffer.toList
        }

        /*
          * Traverse webElement and the correspondingUnevaluated Expr at the same time.
          * Creates a tree corresponding to webElement, but made of WebElementWithID.
          * Add the mappings Id -> UnevaluatedExpr to sourceMap
          *
          * @param sourceMap
          * @param webElement
          * @param correspondingUnevaluatedExpr
          * @return
          */
        def giveIDToWebElementsAndFillSourceMap(id: Int, webElement: WebElement, correspondingUnevaluatedExpr: Expr, sourceMap: SourceMap, sReporter: ServerReporter) : (WebElementWithID, Int) = {
          //sReporter.report(Info, "Processing: webElement: "+webElement+" and corresponding unevaluated Expr: "+correspondingUnevaluatedExpr)
          def sanityCheck(webElement: WebElement, correspondingUnevaluatedExpr: Expr, caseClassDef: CaseClassDef, webElementName: String, sReporter:ServerReporter): Unit = {
            correspondingUnevaluatedExpr match {
              case CaseClass(CaseClassType(`caseClassDef`, targs), args) => ()/*correspondingUnevaluatedExpr*/
              //              case TupleSelectOrCaseClassSelect(actualExpr)=>
              //                sanityCheck(webElement, actualExpr, caseClassDef, webElementName, sReporter)
              case _ =>
                sReporter.report(Error,
                  s"""When IDing the webElements and building the sourceMap, function giveIDToWebElementsAndFillSourceMap was given a $webElementName and an expr that did not represent a $webElementName:
                      |   $webElementName: $webElement
                      |   Expr: $correspondingUnevaluatedExpr
                  """.stripMargin)
              //                TODO: throw an exception instead of the following line
              //                correspondingUnevaluatedExpr
            }
          }
          val actualCorrespondingUnevaluatedExpr = TupleSelectAndCaseClassSelectRemover.removeTopLevelTupleSelectsAndCaseClassSelects(correspondingUnevaluatedExpr)
          webElement match {
            // Wildcard patterns are not used for most of the following cases, so that the compiler complains whenever
            // the types of the arguments of these case classes are changed (in their definition).
            // Because the following code may go haywire if these types are changed (especially if WebElements are added
            // to the definitions of these case classes)
            case WebElementWithID(_,_) =>
              //              Should never happen
              sReporter.report(Error,
                s"""Something went wrong, function giveIDToWebElementsAndFillSourceMap was given a WebElementWithID:
                    |   WebElementWithID: $webElement
                    |   Expr: $actualCorrespondingUnevaluatedExpr
                  """.stripMargin)
              (WebElementWithID(webElement, 0), id)
            case TextElement(text: String) =>
              sanityCheck(webElement, actualCorrespondingUnevaluatedExpr, cb.textElementCaseClassDef, "TextElement", sReporter)
              sourceMap.addMapping(id, webElement, actualCorrespondingUnevaluatedExpr)
              (WebElementWithID(webElement, id), id+1)
            case Element(tag: String, sons, properties, styles) =>
              sanityCheck(webElement, actualCorrespondingUnevaluatedExpr, cb.elementCaseClassDef, "Element", sReporter)
              actualCorrespondingUnevaluatedExpr match {
                case CaseClass(CaseClassType(cb.elementCaseClassDef, targs), List(argTag, argSons, argProperties, argStyles)) =>
                  sourceMap.addMapping(id, webElement, actualCorrespondingUnevaluatedExpr)
                  val sonsWebElemCorrespUnevalExprCouplLeonList = sons.zip(cb.exprOfLeonListOfExprToLeonListOfExpr(argSons))
                  var newId = id + 1
                  val iDedSons : leon.collection.List[WebElement]= sonsWebElemCorrespUnevalExprCouplLeonList.map(
                    {case (webElem, correspUnevalExpr) =>
                      val (w, idAfterChild) = giveIDToWebElementsAndFillSourceMap(newId, webElem, correspUnevalExpr, sourceMap, sReporter)
                      newId = idAfterChild
                      w
                    }
                  )
                  (WebElementWithID(Element(tag, iDedSons, properties, styles), id), newId)
                case e => throw new Exception("Did not pattern match Element")
              }
          }
        }

        /*
          * Traverse webElement
          * Creates a tree corresponding to webElement, but made of WebElementWithID.
          *
          * @param sourceMap
          * @param webElement
          * @param correspondingUnevaluatedExpr
          * @return the webelement with ID along with a fresh ID number
          */
        def giveIDToWebElement(id: Int, webElement: WebElement, sReporter: ServerReporter) : (WebElementWithID, Int) = {
          webElement match {
            // Wildcard patterns are not used for most of the following cases, so that the compiler complains whenever
            // the types of the arguments of these case classes are changed (in their definition).
            // Because the following code may go haywire if these types are changed (especially if WebElements are added
            // to the definitions of these case classes)
            case WebElementWithID(_,_) =>
              //              Should never happen
              sReporter.report(Error,
                s"""Something went wrong, function giveIDToWebElement was given a WebElementWithID:
                    |   WebElementWithID: $webElement
                  """.stripMargin)
              (WebElementWithID(webElement, 0), id)
            case TextElement(text: String) =>
              (WebElementWithID(webElement, id), id+1)
            case Element(tag: String, sons, properties, styles) =>
              var newId = id+1
              val iDedSons = sons.map{ webElem =>
                val (res, newIdAfterChild) = giveIDToWebElement(newId, webElem, sReporter)
                newId = newIdAfterChild
                (res: WebElement)
              }
              (WebElementWithID(Element(tag, iDedSons, properties, styles), id), newId)
          }
        }
        def scalaListToLeonList[T](input: List[T], resultUnderConstruction: leon.collection.List[T] = leon.collection.List[T]()) : leon.collection.List[T]= {
          input match {
            case head :: tail =>
              scalaListToLeonList[T](tail, Cons[T](head, resultUnderConstruction))
            case List() =>
              resultUnderConstruction.reverse
          }
        }

        val webPageWithIDedElements = WebPageWithIDedWebElements(giveIDToWebElement(0, bootstrapWebElement, sReporter)._1, bootstrapcss)
        val computeSourceMap = (resultEvaluationTreeExpr: Expr) => {
          val sourceMap = new SourceMap
          val (bootstrapExprOfUnevaluatedWebElement, bootstrapExprOfUnevaluatedStyle)= resultEvaluationTreeExpr match {
            case CaseClass(CaseClassType(cb.webPageCaseClassDef, targs_1), Seq(argwebpage, argwebstyle)) =>
              (argwebpage, argwebstyle)
          }
          giveIDToWebElementsAndFillSourceMap(0, bootstrapWebElement, bootstrapExprOfUnevaluatedWebElement, sourceMap, sReporter)
          sourceMap
        }

        (webPageWithIDedElements, computeSourceMap)
      }

      //WebPage without the contained WebElement having proper IDs
      val ssReporter = sReporter.startFunction("Unexpring WebPage Expr: "+"DISABLED (to re-enable it, look for \"#VERBOSITY\" in ProgramEvaluator.scala)")
      //      #VERBOSITY
      //    val ssReporter = sReporter.startFunction("Unexpring WebPage Expr: "+webPageEvaluatedExpr)
      val webPage = unExpr(ssReporter)(webPageEvaluatedExpr).asInstanceOf[WebPage]
      //webPageEvaluationTreeExpr
      val (webPageWithIDedWebElements, sourceMapMaker) = buildSourceMapAndGiveIDsToWebElements(webPage, sourceCode, program, sReporter)

      sReporter.report(Info, "Ids assigned to the webElements of the webPage")
      sReporter.report(Info, "WebPageWithIDedWebElements: " + "DISABLED (to re-enable it, look for \"#VERBOSITY\" in ProgramEvaluator.scala)")
      //      #VERBOSITY
      //      sReporter.report(Info, "WebPageWithIDedWebElements: " + webPageWithIDedWebElements.toString)

      val programEvaluationResult = (webPageWithIDedWebElements, sourceMapMaker)
      //      #VERBOSITY
      //      sReporter.report(Info, "Program evaluation result after unExpr: " + programEvaluationResult)
      Left(programEvaluationResult)
    }
    catch {
      case ConversionException(errorString) => {
        Right(errorString)
      }
    }
    result match {
      case Left((webPageWithIDedWebElements, completeSourceMapMaker)) =>
        sReporter.report(Info, "Conversion and SourceMap programming building successful")
        Some((webPageWithIDedWebElements, completeSourceMapMaker))
      case Right(errorString) =>
        sReporter.report(Error, "Conversion and SourceMap building failed: " + errorString)
        None
    }
  }
}
