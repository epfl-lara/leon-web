package leon.web
package shared
import leon.webDSL.webDescription.{WebPageWithIDedWebElements}

case class StringPositionInSourceCode(lineFrom: Int, colFrom: Int,
lineTo: Int, colTo: Int)

case class StringModificationSubmissionConcResult(
    source: String,
    elementsChanged: List[StringPositionInSourceCode],
    newSourceId: Int,
    webPage: WebPageWithIDedWebElements)

/**
  * Created by dupriez on 3/10/16.
  */
case class StringModificationSubmissionResult(
    newSourceCodeAndWebPage: Option[StringModificationSubmissionConcResult],
    log: String)
