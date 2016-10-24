/* Copyright 2009-2015 EPFL, Lausanne */

package leon.web.shared

sealed abstract class RepositoryType(val id: String) {
  override def toString = id
}

object RepositoryType {

  case object Local   extends RepositoryType("local")
  case object GitHub  extends RepositoryType("github")
  case object Tequila extends RepositoryType("tequila")
  case object Unknown extends RepositoryType("unknown")

  private val all = Seq(
      Local, GitHub, Tequila, Unknown
    )
    .map(t => (t.id, t))
    .toMap

  def apply(id: String): RepositoryType =
    all.getOrElse(id, Unknown)
}

sealed trait RepositoryDesc {
  def provider: Provider
  def desc: String

  override def toString = s"$provider:$desc"
}

object RepositoryDesc {

  case class GitHub(owner: String, name: String) extends RepositoryDesc {
    val desc     = s"$owner/$name"
    val provider = Provider.GitHub
  }

  case class Tequila(sciper: String, name: String) extends RepositoryDesc {
    val desc     = s"$sciper/$name"
    val provider = Provider.Tequila
  }

  case class Local(path: String) extends RepositoryDesc {
    val desc     = path
    val provider = Provider.Local
  }

  def fromGitHub(owner: String, repo: String) =
    RepositoryDesc.GitHub(owner, repo)

  def fromTequila(sciper: String, name: String) =
    RepositoryDesc.Tequila(sciper, name)

  def fromLocal(path: String) =
    RepositoryDesc.Local(path)
}

sealed trait Visibility { def name: String }

object Visibility {

  case object Public  extends Visibility { val name = "public" }
  case object Private extends Visibility { val name = "private" }
  case object All     extends Visibility { val name = "all" }

  def apply(name: String) = name match {
    case Public.name  => Public
    case Private.name => Private
    case _            => All
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

case class GitHubRepositoryId(value: Long)

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

case class TequilaRepository(
  sciper        : String,
  name          : String,
  defaultBranch : String,
  branches      : Seq[Branch]
) extends Repository {
  val desc       = RepositoryDesc.fromTequila(sciper, name)
  val cloneURL   = ""
  val visibility = Visibility.Private
  val fullName   = name
  val fork       = false
}

case class LocalRepository(
  name          : String,
  path          : String,
  defaultBranch : String,
  branches      : Seq[Branch]
) extends Repository {
  val desc       = RepositoryDesc.fromLocal(path)
  val cloneURL   = ""
  val visibility = Visibility.Private
  val fullName   = name
  val fork       = false
}

case class RepositoryId(value: Long)

