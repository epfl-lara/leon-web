package leon.web
package models

import java.io.File
import scala.io.Source
import java.util.zip.ZipFile
import java.util.zip.ZipInputStream
import scala.collection.mutable.ArrayBuffer

class FileExamples(subdir: String) {
  def titleOf(fname: String): String = {
    val name = fname.substring(0, fname.lastIndexOf('.'))

    name.split("/").last.split("_").toList.tail.mkString(" ")
  }

  lazy val allExamples: List[Example] = {
    val target = "resources/examples/"+subdir
    val d = this.getClass.getResource("/"+target)

    if (d != null) {
      d.getProtocol() match {
        case "jar" =>
          val examples = new ArrayBuffer[(String, Example)]()

          val src = this.getClass.getProtectionDomain().getCodeSource();
          if (src ne null) {
            val zl = src.getLocation()
            val zf = new ZipFile(zl.getPath())
            val zis = new ZipInputStream(zl.openStream())
            var ze = zis.getNextEntry()
            while(ze ne null) {
              val name = ze.getName()
              if (name.startsWith(target) && name.size > target.size+1) {
                val content = Source.fromInputStream(zf.getInputStream(ze)).mkString

                examples += (name -> Example(titleOf(name), content))
              }
              ze = zis.getNextEntry()
            }

            examples.toList.sortBy(_._1).map(_._2)
          } else {
            Nil
          }
        case "file" =>
          val asFile = new File(d.toURI())
          asFile.listFiles().sortBy(_.getPath()).toList.map { case f =>
            Example(titleOf(f.getName), Source.fromFile(f.getPath).mkString)
          }
      }
    } else {
      Nil
    }
  }
}
