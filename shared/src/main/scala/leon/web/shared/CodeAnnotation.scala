package leon.web.shared

case class CodeAnnotation(line: Int, col: Int, message: String, var tpe: CodeAnnotationType)

sealed trait CodeAnnotationType { def kind: String }

case object CodeAnnotationError        extends CodeAnnotationType { val kind = "error" }
case object CodeAnnotationWarning      extends CodeAnnotationType { val kind = "warning" }
case object CodeAnnotationSynthesis    extends CodeAnnotationType { val kind = "synthesis" }
case object CodeAnnotationVerification extends CodeAnnotationType { val kind = "verification" }