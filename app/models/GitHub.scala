/* Copyright 2009-2015 EPFL, Lausanne */

package leon.web
package models

import play.api.libs.json._
import play.api.libs.functional.syntax._

/** Models for a subset of the [[https://developer.github.com/v3/ GitHub API]] */
object github {

  sealed trait Visibility
  case object Public  extends Visibility
  case object Private extends Visibility
  case object All     extends Visibility

  sealed trait Affiliation
  case object Owner              extends Affiliation
  case object Collaborator       extends Affiliation
  case object OrganizationMember extends Affiliation

  case class Branch(name: String, sha: String)

  case class RepositoryId(value: Long) extends AnyVal

  case class Repository(
    id            : RepositoryId,
    name          : String,
    fullName      : String,
    owner         : String,
    visibility    : Visibility,
    fork          : Boolean,
    size          : Long,
    cloneURL      : String,
    defaultBranch : String,
    branches      : Seq[Branch]
  )

  /** JSON Reads and Writes instances for [[leon.web.models.github]] models */
  object json {

    implicit val repositoryIdWrites: Writes[RepositoryId] = Writes {
      id => JsNumber(id.value)
    }

    implicit val repositoryIdReads: Reads[RepositoryId] = Reads {
      _.validate[Long].map(RepositoryId(_))
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

    implicit val affiliationWrites: Writes[Affiliation] = Writes {
      case Owner              => JsString("owner")
      case Collaborator       => JsString("collaborator")
      case OrganizationMember => JsString("organization_member")
    }

    implicit val affiliationReads: Reads[Affiliation] = Reads {
      _.validate[String].map {
        case "owner"               => Owner
        case "collaborator"        => Collaborator
        case "organization_member" => OrganizationMember
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

    implicit val repositoryWrites: Writes[Repository] = (
      ( __ \ "id"            ).write[RepositoryId] and
      ( __ \ "name"          ).write[String]       and
      ( __ \ "fullName"      ).write[String]       and
      ( __ \ "owner"         ).write[String]       and
      ( __ \ "visibility"    ).write[Visibility]   and
      ( __ \ "fork"          ).write[Boolean]      and
      ( __ \ "size"          ).write[Long]         and
      ( __ \ "cloneURL"      ).write[String]       and
      ( __ \ "defaultBranch" ).write[String]       and
      ( __ \ "branches"      ).write[Seq[Branch]]
    )(unlift(Repository.unapply))

    implicit val repositoryReads: Reads[Repository] = (
      ( __ \ "id"              ).read[RepositoryId] and
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
    )(Repository.apply _)
  }

}

