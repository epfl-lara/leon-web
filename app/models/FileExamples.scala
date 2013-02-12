package leon.web
package models

import java.io.File
import scala.io.Source

class FileExamples(subdir: String) {
  lazy val allExamples: List[Example] = {
    val d = this.getClass.getResource("/resources/examples/"+subdir)

    if (d != null) {
      val asFile = new File(d.toURI())

      asFile.listFiles().sortBy(_.getPath()).toList.map{ f =>
        // files are "XX_Name_More.scala
        var name = f.getName
        // remove extension
        name = name.substring(0, name.lastIndexOf('.'))

        // remove ordering number, and replace '_' by " "
        val title = name.split("_").toList.tail.mkString(" ")

        Example(title, Source.fromFile(f.getPath).mkString)
      }
    } else {
      Nil
    }
  }
}
