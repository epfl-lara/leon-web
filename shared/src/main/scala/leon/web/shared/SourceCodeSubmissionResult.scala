package leon.web
package shared

import leon.webDSL.webDescription.{WebPage, WebPageWithIDedWebElements}

/**
  * Created by dupriez on 3/10/16.
  */
case class SourceCodeSubmissionResult(webPageWithIDedWebElementsOpt: Option[WebPageWithIDedWebElements], evaluationLog: String)