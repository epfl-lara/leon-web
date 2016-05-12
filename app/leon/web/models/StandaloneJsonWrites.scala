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
import leon.web.shared.LocalRepository
import scala.math.BigDecimal.long2bigDecimal

trait StandaloneJsonWrites {

  import leon.web.shared._

  implicit val providerWrites: Writes[Provider] = Writes { p =>
    JsString(p.id)
  }

  implicit val visibilityWrites: Writes[Visibility] = Writes {
    case Public  => JsString("public")
    case Private => JsString("private")
    case All     => JsString("all")
  }

  implicit val visibilityReads: Reads[Visibility] = Reads {
    _.validate[Boolean].map {
      case false => Public
      case true  => Private
    }
  }

  implicit val branchWrites: Writes[Branch] = (
    ( __ \ "name" ).write[String] and
    ( __ \ "sha"  ).write[String]
  )(unlift(Branch.unapply))

  implicit val branchReads: Reads[Branch] = (
    ( __ \ "name"           ).read[String] and
    ( __ \ "commit" \ "sha" ).read[String]
  )(Branch.apply _)

  implicit val repositoryIdWrites: Writes[GitHubRepositoryId] = Writes {
    id => JsNumber(id.value)
  }

  implicit val repositoryIdReads: Reads[GitHubRepositoryId] = Reads {
    _.validate[Long].map(GitHubRepositoryId(_))
  }

  implicit val githubRepositoryWrites: Writes[GitHubRepository] = (
    ( __ \ "id"            ).write[GitHubRepositoryId] and
    ( __ \ "name"          ).write[String]       and
    ( __ \ "fullName"      ).write[String]       and
    ( __ \ "owner"         ).write[String]       and
    ( __ \ "visibility"    ).write[Visibility]   and
    ( __ \ "fork"          ).write[Boolean]      and
    ( __ \ "size"          ).write[Long]         and
    ( __ \ "cloneURL"      ).write[String]       and
    ( __ \ "defaultBranch" ).write[String]       and
    ( __ \ "branches"      ).write[Seq[Branch]]
  )(unlift(GitHubRepository.unapply))

  implicit val githubRepositoryReads: Reads[GitHubRepository] = (
    ( __ \ "id"              ).read[GitHubRepositoryId] and
    ( __ \ "name"            ).read[String]       and
    ( __ \ "full_name"       ).read[String]       and
    ( __ \ "owner" \ "login" ).read[String]       and
    ( __ \ "private"         ).read[Visibility]   and
    ( __ \ "fork"            ).read[Boolean]      and
    ( __ \ "size"            ).read[Long]         and
    ( __ \ "clone_url"       ).read[String]       and
    ( __ \ "default_branch"  ).read[String]       and
    ( __ \ "branches"        ).readNullable[Seq[Branch]]
                              .map(_.getOrElse(Seq()))
  )(GitHubRepository.apply _)

  implicit val localRepositoryWrites: Writes[LocalRepository] = (
    ( __ \ "name"          ).write[String]       and
    ( __ \ "cloneURL"      ).write[String]       and
    ( __ \ "defaultBranch" ).write[String]       and
    ( __ \ "branches"      ).write[Seq[Branch]]
  )(unlift(LocalRepository.unapply))

  implicit val localRepositoryReads: Reads[LocalRepository] = (
    ( __ \ "name"            ).read[String]       and
    ( __ \ "clone_url"       ).read[String]       and
    ( __ \ "default_branch"  ).read[String]       and
    ( __ \ "branches"        ).readNullable[Seq[Branch]]
                              .map(_.getOrElse(Seq()))
  )(LocalRepository.apply _)

  implicit val repositoryWrites: Writes[Repository] = Writes {
    case local: LocalRepository   => toJson(local)(localRepositoryWrites)
    case github: GitHubRepository => toJson(github)(githubRepositoryWrites)
  }

}

object StandaloneJsonWrites extends StandaloneJsonWrites

