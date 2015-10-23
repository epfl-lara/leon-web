package leon.web
package models

object GitHub {

  sealed trait Visibility
  case object Public  extends Visibility
  case object Private extends Visibility
  case object All     extends Visibility

  sealed trait Affiliation
  case object Owner              extends Affiliation
  case object Collaborator       extends Affiliation
  case object OrganizationMember extends Affiliation

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
    defaultBranch : String
  )

}

