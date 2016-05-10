package leon.web.shared.github

sealed trait Visibility { def name: String }
case object Public  extends Visibility { val name = "public" }
case object Private extends Visibility { val name = "private" }
case object All     extends Visibility { val name = "all" }

object Visibility {
  def apply(name: String) = name match {
    case Public.name => Public
    case Private.name => Private
    case _ => All
  }
  def unapply(v: Visibility) = Some(v.name)
}

sealed trait Affiliation
case object Owner              extends Affiliation
case object Collaborator       extends Affiliation
case object OrganizationMember extends Affiliation

case class Branch(name: String, sha: String)

case class RepositoryId(value: Long)// extends AnyVal

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