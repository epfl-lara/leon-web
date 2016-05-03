/* Copyright 2009-2015 EPFL, Lausanne */

package leon.web
package shared

trait RepositoryDesc {
  def desc: String
  def ofType: RepositoryType

  override def toString = desc
}

object RepositoryDesc {

  case class GitHub(owner: String, name: String) extends RepositoryDesc {
    val desc   = s"$owner/$name"
    val ofType = RepositoryType.GitHub
  }

  case class Local(path: String) extends RepositoryDesc {
    val desc   = path
    val ofType = RepositoryType.Local
  }

  import RepositoryType._

  def fromGitHub(owner: String, repo: String) = GitHub(owner, repo)
  def fromLocal(path: String) = Local(path)
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
  def cloneURL: String
  def defaultBranch: String
  def branches: Seq[Branch]
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
  val desc = RepositoryDesc.fromGitHub(owner, name)
}

case class LocalRepository(
  path          : String,
  cloneURL      : String,
  defaultBranch : String,
  branches      : Seq[Branch]
) extends Repository {
  val desc = RepositoryDesc.fromLocal(path)
}

