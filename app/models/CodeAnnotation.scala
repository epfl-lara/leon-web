package leon.web
package models

import play.api.libs.json._

case class CodeAnnotation(line: Int, col: Int, message: String, tpe: CodeAnnotationType) {
  def toJson: JsValue = {
    Json.toJson(Map(
      "row"     -> Json.toJson(line-1),
      "column"  -> Json.toJson(col-1),
      "text"    -> Json.toJson(message),
      "type"    -> tpe.toJson
    ))
  }
}

abstract class CodeAnnotationType(kind: String) {
  val toJson = Json.toJson(kind)
}

case object CodeAnnotationError        extends CodeAnnotationType("error")
case object CodeAnnotationSynthesis    extends CodeAnnotationType("synthesis")
case object CodeAnnotationVerification extends CodeAnnotationType("verification")
