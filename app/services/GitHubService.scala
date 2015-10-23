package leon.web
package services

import play.api.Play.current
import play.api.libs.json._
import play.api.libs.ws._
import play.api.libs.ws.ning.NingAsyncHttpClientConfigBuilder
import scala.concurrent.{Future, ExecutionContext}

object github {

  import leon.web.models.GitHub._
  import leon.web.json.GitHub._

  trait GitHubService {
    def listUserRepositories(): Future[Seq[Repository]]
  }

  object GitHubService {
    def apply(token: String)(implicit ec: ExecutionContext): GitHubService =
      new WSGitHubService(token)
  }

  class WSGitHubService(token: String)(implicit ec: ExecutionContext) extends GitHubService {

    private val baseURL = "https://api.github.com"

    private def req(url: String) =
      WS.url(s"$baseURL$url")
        .withHeaders("Authorization" -> s"token $token")
        .withHeaders("Accept" -> "application/vnd.github.v3+json")

    override def listUserRepositories(): Future[Seq[Repository]] = {
      req("/user/repos").get() map { res =>
        res.json.validate[Seq[Repository]] match {
          case s: JsSuccess[Seq[Repository]] => s.get
          case e: JsError =>
            println("Errors: " + JsError.toFlatJson(e).toString())
            Seq()
        }
      }
    }


  }

}

