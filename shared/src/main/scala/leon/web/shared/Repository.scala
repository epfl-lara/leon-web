/* Copyright 2009-2015 EPFL, Lausanne */

package leon.web.shared

sealed trait RepositoryDesc {
  def desc: String
  def ofType: RepositoryType

  override def toString = desc
}

case class GitHubRepositoryDesc(owner: String, name: String) extends RepositoryDesc {
  val desc   = s"$owner/$name"
  val ofType = GitHubRepositoryType
}

case class LocalRepositoryDesc(path: String) extends RepositoryDesc {
  val desc   = path
  val ofType = LocalRepositoryType
}

object RepositoryDesc {

  import RepositoryType._

  def fromGitHub(owner: String, repo: String) = GitHubRepositoryDesc(owner, repo)
  def fromLocal(path: String) = LocalRepositoryDesc(path)
}

sealed abstract class RepositoryType(val id: String)

case object LocalRepositoryType   extends RepositoryType("local")
case object GitHubRepositoryType  extends RepositoryType("github")
case object UnknownRepositoryType extends RepositoryType("unknown")

object RepositoryType {

  def apply(id: String): RepositoryType = id match {
    case "local"  => LocalRepositoryType
    case "github" => GitHubRepositoryType
    case _        => UnknownRepositoryType
  }
}

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

sealed trait Repository {
  def cloneURL: String
  def defaultBranch: String
  def branches: Seq[Branch]
  def desc: RepositoryDesc
  def visibility: Visibility
  def fullName: String
  def fork: Boolean
}

case class GitHubRepositoryId(value: Long)// extends AnyVal

case class GitHubRepository(
  id            : GitHubRepositoryId,
  name          : String,
  fullName      : String,
  owner         : String,
  visibility    : Visibility,
  fork          : Boolean,
  size          : Long,
  cloneURL      : String,
  defaultBranch : String,
  branches      : Seq[Branch]
) extends Repository {
  val desc = RepositoryDesc.fromGitHub(owner, name)
}

case class LocalRepository(
  path          : String,
  cloneURL      : String,
  defaultBranch : String,
  branches      : Seq[Branch]
) extends Repository {
  val desc = RepositoryDesc.fromLocal(path)
  val visibility = Private
  val fullName   = path
  val fork = false
}

case class RepositoryId(value: Long)// extends AnyVal

