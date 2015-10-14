package leon.web
package models

import play.api._

case class LeonWebConfig(
  examples: List[(String, List[Example])],
  default: leon.web.models.Example,
  url: String,
  isSSL: Boolean,
  release: String
)

object LeonWebConfig {
  def fromCurrent(exs: List[(String, List[Example])]) = {
    val url = Play.current.configuration.getString("app.url").getOrElse("/")
    val ssl = Play.current.configuration.getBoolean("app.ssl").getOrElse(true)
    val subexamples = exs.tail.headOption.getOrElse(exs.head)._2
    if (subexamples.isEmpty) { // Synthesis example first, else default examples
      None
    } else {
      Some(LeonWebConfig(exs, subexamples.head, url, ssl, getLeonRelease))
    }
  }

  def getLeonRelease: String = {
    import java.io.File
    import scala.io.Source

    val f = new File("./version")
    if (f.isFile) {
      Source.fromFile(f).getLines.toList.headOption.getOrElse("")
    } else {
      ""
    }
  }
}
