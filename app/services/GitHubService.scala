/* Copyright 2009-2015 EPFL, Lausanne */

package leon.web
package services

import play.api.Play.current
import play.api.libs.json._
import play.api.libs.ws._
import play.api.libs.ws.ning.NingAsyncHttpClientConfigBuilder
import scala.concurrent.{Future, ExecutionContext}

object github {

  import leon.web.models.github._
  import leon.web.models.github.json._

  /** Defines an interface to the GitHub API */
  trait GitHubService {
    type Error = String

    /** List the logged-in user's repositories. */
    def listUserRepositories(): Future[Either[Error, Seq[Repository]]]

    /** Retrieve information about a specific repository, that must
      * must be accessible by the currently logged-in user.
      *
      * @param owner the owner of the repository (can differ from logged-in user)
      * @param name the name of the repository
      */
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
          Left(error)
      }

    // TODO: Follow the pagination to load all repositories
    override def listUserRepositories(): Future[Either[Error, Seq[Repository]]] = {
      req("/user/repos")
        .withQueryString("affiliation" -> "owner", "per_page" -> "100")
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

