package leon.web
package models

import leon.web.shared.Provider

import play.api._

case class LeonWebConfig(
  examples: List[(String, List[Example])],
  default: leon.web.models.Example,
  url: String,
  isSSL: Boolean,
  release: String
) {

  lazy val enabledLoginProviders: List[Provider] = {
    val c = Play.current.configuration

    def isEnabled(provider: Provider) = {
      val id     = c.getString(s"auth.${provider.id}.clientId")
      val secret = c.getString(s"auth.${provider.id}.clientSecret")
      id.exists(_.nonEmpty) && secret.exists(_.nonEmpty)
    }

    Provider.all.toList.filter(isEnabled)
  }

}

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
