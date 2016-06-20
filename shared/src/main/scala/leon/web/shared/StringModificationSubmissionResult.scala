package shared
import leon.webDSL.webDescription.{WebPageWithIDedWebElements}

case class StringPositionInSourceCode(lineFrom: Int, colFrom: Int, lineTo: Int, colTo: Int)

//case class StringModificationSubmissionConcResult(source: String,
//                                                  elementsChanged: List[StringPositionInSourceCode],
//                                                  newSourceId: Int,
//                                                  webPage: WebPageWithIDedWebElements)

case class ShippableClarificationSolution(
                                           sourceCode: String,
                                           idedWebPage: WebPageWithIDedWebElements,
                                           positionsOfModificationsInSourceCode: List[StringPositionInSourceCode],
                                           textContentOfClarifiedWebElementOption: Option[String] //Will be None if no other solutions were found.
                                         )

case class  PotentialWebPagesList(
                                   newSourceCodeID: Option[Int], //NewSourceCodeID will be None if StringModificationProcessor did not find any possible sourceCode/WebPage
                                   solutionList: List[ShippableClarificationSolution],
                                   idOfClarifiedWebElementOption: Option[Int]
                                   //ID of the WebElement on which the clarification is being performed
                                   //Will be None if there is one or no solutions
                                 )

/**
  * Created by dupriez on 3/10/16.
  */

//The reason why StringModificationSubmissionResult always contains a PotentialWebPagesList, which may or may not contain potential webPages,
// instead of an Option[PotentialWebPageList] is because boopickle complained that it cannot pickle Option[PotentialWebPageList]
case class StringModificationSubmissionResult(
                                               suggestions: PotentialWebPagesList,
                                               //                                             b: StringModificationSubmissionConcResult,
                                               //                                             c: Either[StringModificationSubmissionConcResult, String],
                                               //                                             questionOrFinalSolution: Option[Either[ClarificationQuestion, StringModificationSubmissionConcResult]],
                                               //    newSourceCodeAndWebPage: Option[StringModificationSubmissionConcResult],
                                               log: String)
