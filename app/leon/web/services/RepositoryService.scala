/* Copyright 2009-2015 EPFL, Lausanne */

package leon.web
package services

import scala.concurrent.{Future, ExecutionContext}
import leon.web.models.User
import leon.web.shared.{RepositoryDesc, Repository, RepositoryType}

class RepositoryService(user: User, ghService: Option[GitHubService]) {

  import RepositoryDesc._

  def fromDesc(desc: RepositoryDesc)(implicit ec: ExecutionContext): Future[Repository] = desc match {
    case Local(path) =>
      Future.successful(getLocalRepository(path))

    case GitHub(owner, name) if ghService.isDefined =>
      ghService.get.getRepository(owner, name)

    case GitHub(owner, name) =>
      Future.failed(new Throwable("Missing GitHub oAuth token."))
  }

  def getLocalRepository(path: String): Repository = ???

}

object RepositoryService {

  def apply(user: User) = {
    val token     = user.github.flatMap(_.oAuth2Info).map(_.accessToken)
    val ghService = token.map(GitHubService(_))

    new RepositoryService(user, ghService)
  }

  import play.api.libs.json._

  def parseRepositoryDesc(json: JsValue): Option[RepositoryDesc] = {
    import RepositoryType._

    val ofType = (json \ "type").as[String]

    RepositoryType(ofType) match {
      case GitHub =>
        val owner = (json \ "owner").as[String]
        val name  = (json \ "name").as[String]
        Some(RepositoryDesc.fromGitHub(owner, name))

      case Local =>
        val path = (json \ "path").as[String]
        Some(RepositoryDesc.fromLocal(path))

      case Unknown =>
        None
    }
  }

}

