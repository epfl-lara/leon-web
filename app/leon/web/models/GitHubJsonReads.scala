package leon.web
package models

import play.api.libs.json._
import play.api.libs.json.Json._
import play.api.libs.functional.syntax._
import leon._
import leon.evaluators._
import leon.verification._
import leon.utils._
import leon.purescala.Common._
import leon.purescala.Expressions._
import leon.purescala.Definitions._
import leon.web.shared.Branch
import leon.web.shared.GitHubRepository
import leon.web.shared.GitHubRepositoryId

object GitHubJsonReads {

  import leon.web.shared._

  implicit val visibilityReads: Reads[Visibility] = Reads {
    _.validate[Boolean].map {
      case false => Visibility.Public
      case true  => Visibility.Private
    }
  }
  implicit val branchReads: Reads[Branch] = (
    ( __ \ "name"           ).read[String] and
    ( __ \ "commit" \ "sha" ).read[String]
  )(Branch.apply _)

  implicit val repositoryIdReads: Reads[GitHubRepositoryId] = Reads {
    _.validate[Long].map(GitHubRepositoryId(_))
  }

  implicit val githubRepositoryReads: Reads[GitHubRepository] = (
    ( __ \ "id"              ).read[GitHubRepositoryId] and
    ( __ \ "name"            ).read[String]             and
    ( __ \ "owner" \ "login" ).read[String]             and
    ( __ \ "private"         ).read[Visibility]         and
    ( __ \ "fork"            ).read[Boolean]            and
    ( __ \ "size"            ).read[Long]               and
    ( __ \ "clone_url"       ).read[String]             and
    ( __ \ "default_branch"  ).read[String]             and
    ( __ \ "branches"        ).readNullable[Seq[Branch]]
                              .map(_.getOrElse(Seq()))
  )(GitHubRepository.apply _)

}

