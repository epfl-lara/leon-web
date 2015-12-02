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
    type Error <: Throwable

    /** List the logged-in user's repositories. */
    def listUserRepositories(): Future[Seq[Repository]]

    /** Retrieve information about a specific repository, that must
      * must be accessible by the currently logged-in user.
      *
      * @param owner the owner of the repository (can differ from logged-in user)
      * @param name  the name of the repository
      */
    def getRepository(owner: String, name: String): Future[Repository]

    /** Retrieve the list of branches available for the given
      * repository, which must be accessible by the currently logged-in user.
      *
      * @param owner the owner of the repository (can differ from logged-in user)
      * @param name  the name of the repository
      */
    def getBranches(owner: String, name: String): Future[Seq[Branch]]
  }

  object GitHubService {
    def apply(token: String)(implicit ec: ExecutionContext): GitHubService =
      new WSGitHubService(token)
  }

  class WSGitHubService(token: String)(implicit ec: ExecutionContext) extends GitHubService {

    case class Error(private val message: String) extends Throwable(message)

    private val baseURL = "https://api.github.com"

    private def req(url: String): WSRequestHolder = {
      WS.url(s"$baseURL$url")
        .withRequestTimeout(10000)
        .withHeaders("Authorization" -> s"token $token")
        .withHeaders("Accept" -> "application/vnd.github.v3+json")
    }

    private def unwrapSuccess[T : Reads](res: WSResponse): Either[Error, T] =
      res.json.validate[T] match {
        case s: JsSuccess[T] =>
          Right(s.get)

        case e: JsError =>
          val error = JsError.toFlatJson(e).toString
          Left(Error(error))
      }

    private def eitherToFuture[T](either: Either[Error, T]): Future[T] = either match {
      case Left(err)  => Future failed err
      case Right(res) => Future successful res
    }

    // TODO: Follow the pagination to load all repositories
    override def listUserRepositories(): Future[Seq[Repository]] = {
      req("/user/repos")
        .withQueryString("affiliation" -> "owner", "per_page" -> "100")
        .get()
        .map(unwrapSuccess[Seq[Repository]])
        .flatMap(eitherToFuture[Seq[Repository]])
    }

    override def getRepository(owner: String, name: String): Future[Repository] = {
      val branchesFuture = getBranches(owner, name)

      for {
        res      <- req(s"/repos/$owner/$name").get()
        repo     <- eitherToFuture[Repository](unwrapSuccess[Repository](res))
        branches <- branchesFuture
      }
      yield repo.copy(branches = branches)
    }

    override def getBranches(owner: String, name: String): Future[Seq[Branch]] = {
      req(s"/repos/$owner/$name/branches")
        .get()
        .map(unwrapSuccess[Seq[Branch]])
        .flatMap(eitherToFuture[Seq[Branch]])
    }

  }

}

