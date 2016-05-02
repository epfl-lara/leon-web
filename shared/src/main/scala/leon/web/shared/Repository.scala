/* Copyright 2009-2015 EPFL, Lausanne */

package leon.web
package shared

trait RepositoryDesc {
  def desc: String
  def ofType: RepositoryType
}

case class GitHubRepositoryDesc(owner: String, name: String) extends RepositoryDesc {
  val desc   = s"$owner/$name"
  val ofType = RepositoryType.GitHub
}

case class LocalRepositoryDesc(path: String) extends RepositoryDesc {
  val desc = path
  val ofType = RepositoryType.Local
}

object RepositoryDesc {
  import RepositoryType._

  def fromGitHub(owner: String, repo: String) = GitHubRepositoryDesc(owner, repo)
  def fromLocal(path: String) = LocalRepositoryDesc(path)
}

sealed abstract class RepositoryType(val id: String)
object RepositoryType {
  case object Local   extends RepositoryType("local")
  case object GitHub  extends RepositoryType("github")
  case object Unknown extends RepositoryType("unknown")

  def apply(id: String): RepositoryType = id match {
    case "local"  => Local
    case "github" => GitHub
    case _        => Unknown
  }
}

sealed trait Visibility
case object Public  extends Visibility
case object Private extends Visibility
case object All     extends Visibility

case class Branch(name: String, sha: String)

trait Repository {
  def desc: RepositoryDesc
}

case class GitHubRepositoryId(value: Long) extends AnyVal

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
  val desc = GitHubRepositoryDesc(owner, name)
}

case class LocalRepository(
  path          : String,
  defaultBranch : String,
  branches      : Seq[Branch]
) extends Repository {
  val desc = LocalRepositoryDesc(path)
}

