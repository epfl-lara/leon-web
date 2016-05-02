/* Copyright 2009-2015 EPFL, Lausanne */

package leon.web
package services

import scala.concurrent.{Future, ExecutionContext}

import leon.web.models.User
import leon.web.shared.{RepositoryDesc, Repository, RepositoryType}

class RepositoryService(user: User, github: GitHubService) {

  import RepositoryDesc._

  def fromDesc(desc: RepositoryDesc)(implicit ec: ExecutionContext): Future[Repository] = desc match {
    case Local(path) =>
      Future.successful(getLocalRepository(path))

    case GitHub(owner, name) =>
      github.getRepository(owner, name)
  }

  def getLocalRepository(path: String): Repository = ???

}

object RepositoryService {

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

