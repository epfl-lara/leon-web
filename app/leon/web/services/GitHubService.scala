/* Copyright 2009-2015 EPFL, Lausanne */

package leon.web
package services

import play.api.Play.current
import play.api.libs.json._
import play.api.libs.ws._
import play.api.libs.ws.ning.NingAsyncHttpClientConfigBuilder
import scala.concurrent.{Future, ExecutionContext}
import shared.github._

import leon.web.shared._

object GitHubService {
  def apply(token: String): GitHubService =
    new GitHubService(token)
}

/** Defines an interface to the GitHub API */
class GitHubService(token: String) {

  import leon.web.models.StandaloneJsonWrites._

  case class Error(private val message: String) extends Throwable(message)

  private val baseURL = "https://api.github.com"

  private def req(url: String): WSRequestHolder = {
    WS.url(s"$baseURL$url")
      .withRequestTimeout(10000)
      .withHeaders("Authorization" -> s"token $token")
      .withHeaders("Accept" -> "application/vnd.github.v3+json")
  }

  /**
    *  List the logged-in user's repositories.
    *
    * @todo: Follow the pagination to load all repositories
    */
  def listUserRepositories()(implicit ec: ExecutionContext): Future[Seq[GitHubRepository]] = {
    req("/user/repos")
      .withQueryString("affiliation" -> "owner", "per_page" -> "100")
      .get()
      .map(unwrapSuccess[Seq[GitHubRepository]])
      .flatMap(eitherToFuture[Seq[GitHubRepository]])
  }

  /** Retrieve information about a specific repository, that must
    * must be accessible by the currently logged-in user.
    *
    * @param owner the owner of the repository (can differ from logged-in user)
    * @param name  the name of the repository
    */
  def getRepository(owner: String, name: String)(implicit ec: ExecutionContext): Future[GitHubRepository] = {
    val branchesFuture = getBranches(owner, name)

    for {
      res      <- req(s"/repos/$owner/$name").get()
      repo     <- eitherToFuture(unwrapSuccess[GitHubRepository](res))
      branches <- branchesFuture
    }
    yield repo.copy(branches = branches)
  }

  /** Retrieve the list of branches available for the given
    * repository, which must be accessible by the currently logged-in user.
    *
    * @param owner the owner of the repository (can differ from logged-in user)
    * @param name  the name of the repository
    */
  def getBranches(owner: String, name: String)(implicit ec: ExecutionContext): Future[Seq[Branch]] = {
    req(s"/repos/$owner/$name/branches")
      .get()
      .map(unwrapSuccess[Seq[Branch]])
      .flatMap(eitherToFuture[Seq[Branch]])
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

}

