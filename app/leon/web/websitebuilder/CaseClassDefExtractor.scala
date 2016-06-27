package leon.web.websitebuilder

import leon.purescala.Definitions.{CaseClassDef, Program}
import leon.web.websitebuilder.logging.OptionValWithLog
import leon.web.websitebuilder.logging.serverReporter.{Info, Error, ServerReporter}

/**
  * Created by dupriez on 26/06/16.
  * Meant to replace "ProgramExtractor", which was inherited by SourceMap for an unknown reason
  */
object CaseClassDefExtractor {
  private def lookupCaseClass(caseClassFullName: String, program: Program): (Option[CaseClassDef], String) = {
    program.lookupCaseClass(caseClassFullName) match {
      case Some(classDef) =>
        (Some(classDef), s"$caseClassFullName: Success,")
      case None =>
        (None, s"$caseClassFullName: Failure,")
    }
  }

  private val caseClassDefNameAndFullNameMap = Map(
    "Element" -> "leon.webDSL.webDescription.Element",
    "TextElement" -> "leon.webDSL.webDescription.TextElement",
    "WebAttribute" -> "leon.webDSL.webDescription.WebAttribute",
    "WebStyle" -> "leon.webDSL.webDescription.WebStyle",
    "WebPage" -> "leon.webDSL.webDescription.WebPage",
    "leonListCons" -> "leon.collection.Cons",
    "leonListNil" -> "leon.collection.Nil"
  )

  def extractCaseClassDefs(program: Program, serverReporter: ServerReporter): Map[String, CaseClassDef] = {
    val sReporter = serverReporter.startFunction("Looking up Case Class Defs")
    case class ExtractingFailure(log: String) extends Exception
    val (log: String, wrappedUpCaseClassDefMap: Map[String, CaseClassDef]) =
      try {
        caseClassDefNameAndFullNameMap.foldLeft(("Looking them up in the program: ", Map[String, CaseClassDef]())){
          case ((log:String, underConstructionMap), (caseClassName: String, caseClassFullName: String)) =>
            lookupCaseClass(caseClassFullName, program) match {
              case (None, failureMessage)=>
                throw ExtractingFailure(log + failureMessage)
              case (Some(caseClassDef), successMessage) =>
                (log + successMessage, underConstructionMap + (caseClassName -> caseClassDef))
            }
        }
      }
    catch {
      case ExtractingFailure(log) =>
        sReporter.report(Error, log)
    }
    sReporter.report(Info, log)
    wrappedUpCaseClassDefMap
  }
}
