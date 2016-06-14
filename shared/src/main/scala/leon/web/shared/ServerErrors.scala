package leon.web
package shared

/**
  * Created by dupriez on 3/10/16.
  */

trait ServerError{
  val text: String
}

case class UnableToFetchBootstrapSourceCode(text: String = "Unable to fetch bootstrap source code") extends ServerError