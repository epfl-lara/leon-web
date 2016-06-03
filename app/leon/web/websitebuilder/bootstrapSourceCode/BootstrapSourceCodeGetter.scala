package leon.web.websitebuilder
package bootstrapSourceCode

import java.io.ByteArrayInputStream

import logging.serverReporter.{Error, Info, ServerReporter}

import scala.io.BufferedSource

/**
  * Created by dupriez on 2/25/16.
  */
object BootstrapSourceCodeGetter {
  val pathToBootstrapSourceCodeFile = "server/app/bootstrapSourceCode/BootstrapSourceCode"

  def getBootstrapSourceCode(serverReporter: ServerReporter): Option[String] = {
    //    val pathToLeonInput = "src/main/scala-2.11/trash.manipulatedFiles/SourceCode.txt"
    val sReporter = serverReporter.startProcess("Getting bootstrap source code")
    val bootstrapSourceCodeFile: Option[BufferedSource] = try {
      Some(scala.io.Source.fromFile(pathToBootstrapSourceCodeFile))
    } catch {
      case e: java.io.FileNotFoundException => {
        //        mainReporter.error("File opening error, \"" + pathToLeonInput + "\" not found")
        sReporter.report(Error, "File opening error, \"" + pathToBootstrapSourceCodeFile + "\" not found")
        None
      }
    }
    bootstrapSourceCodeFile match {
      case None =>
        sReporter.report(Error, "Failure")
        None
      case Some(bufferedSource) =>
        val bootstrapSourceCode = bufferedSource.mkString
        bufferedSource.close
        sReporter.report(Info, "Success")
        Some(bootstrapSourceCode)
    }
  }
}
