package leon.web
package models

import play.api._

import java.io.File
import scala.io.Source
import java.util.zip.ZipFile
import java.util.zip.ZipInputStream
import scala.collection.mutable.ArrayBuffer

class FileExamples(subdir: String) {
  val dir = Play.current.configuration.getString("app.path").getOrElse("./")
  val prefix = "leon/testcases/web/"

  def titleOf(fname: String): String = {
    val name = fname.substring(0, fname.lastIndexOf('.'))

    name.split("/").last.split("_").toList.tail.mkString(" ")
  }

  lazy val allExamples: List[Example] = {
    val d = new File(dir+prefix+subdir)
    if (d.isDirectory) {
      d.listFiles().sortBy(_.getPath()).toList.map { case f =>
        Example(titleOf(f.getName), Source.fromFile(f.getPath).mkString)
      }
    } else {
      Nil
    }
  }
}
