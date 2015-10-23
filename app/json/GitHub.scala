package leon.web
package json

import play.api.libs.json._
import play.api.libs.functional.syntax._

import leon.web.models.GitHub._

object GitHub {

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

  implicit val repositoryWrites: Writes[Repository] = (
    ( __ \ "id"            ).write[RepositoryId] and
    ( __ \ "name"          ).write[String]       and
    ( __ \ "fullName"      ).write[String]       and
    ( __ \ "owner"         ).write[String]       and
    ( __ \ "visibility"    ).write[Visibility]   and
    ( __ \ "fork"          ).write[Boolean]      and
    ( __ \ "size"          ).write[Long]         and
    ( __ \ "cloneURL"      ).write[String]       and
    ( __ \ "defaultBranch" ).write[String]
  )(unlift(Repository.unapply))

  implicit val repositoryReads: Reads[Repository] = (
    ( __ \ "id"             ).read[RepositoryId] and
    ( __ \ "name"           ).read[String]       and
    ( __ \ "full_name"      ).read[String]       and
    ( __ \ "owner"          ).read[String]       and
    ( __ \ "private"        ).read[Visibility]   and
    ( __ \ "fork"           ).read[Boolean]      and
    ( __ \ "size"           ).read[Long]         and
    ( __ \ "clone_url"      ).read[String]       and
    ( __ \ "default_branch" ).read[String]
  )(Repository.apply _)
}

