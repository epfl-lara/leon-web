package leon.web.shared

case class CodeAnnotation(line: Int, col: Int, message: String, tpe: CodeAnnotationType)

abstract class CodeAnnotationType(kind: String)

case object CodeAnnotationError        extends CodeAnnotationType("error")
case object CodeAnnotationWarning      extends CodeAnnotationType("warning")
case object CodeAnnotationSynthesis    extends CodeAnnotationType("synthesis")
case object CodeAnnotationVerification extends CodeAnnotationType("verification")