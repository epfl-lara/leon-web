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

  case class GitHubServiceError(message: String)

  trait GitHubService {
    type Error = GitHubServiceError

    def listUserRepositories(): Future[Either[Error, Seq[Repository]]]
    def getRepository(owner: String, name: String): Future[Either[Error, Repository]]
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

    private def unwrapSuccess[T : Reads](res: WSResponse): Either[Error, T] =
      res.json.validate[T] match {
        case s: JsSuccess[T] =>
          Right(s.get)

        case e: JsError =>
          val error = JsError.toFlatJson(e).toString()
          Left(GitHubServiceError(error))
      }

    // TODO: Follow the pagination to load all repositories
    override def listUserRepositories(): Future[Either[Error, Seq[Repository]]] = {
      req("/user/repos")
        .withQueryString("affiliation" -> "owner")
        .get()
        .map(unwrapSuccess[Seq[Repository]])
    }

    override def getRepository(owner: String, name: String): Future[Either[Error, Repository]] = {
      req(s"/repos/$owner/$name")
        .get()
        .map(unwrapSuccess[Repository])
    }

  }

}

