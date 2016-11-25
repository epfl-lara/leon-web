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

sealed abstract class RepositoryDesc(val desc: String, val provider: Provider) {
  override def toString = s"$provider:$desc"
}

object RepositoryDesc {

  case class GitHub(owner: String, name: String)
    extends RepositoryDesc(s"$owner/$name", Provider.GitHub)

  case class Tequila(sciper: String, name: String)
    extends RepositoryDesc(s"$sciper/$name", Provider.Tequila)

  case class Local(path: String)
    extends RepositoryDesc(path, Provider.Local)

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

case class Branch(name: String, sha: String)

case class Remote(
  name: String,
  url: String
)

sealed abstract class Repository {
  def name: String
  def fullName: String
  def desc: RepositoryDesc
  def defaultBranch: String
  def branches: Seq[Branch]
  def visibility: Visibility
  def fork: Boolean
  def remote: Option[Remote]
}

case class GitHubRepositoryId(value: Long)

case class GitHubRepository(
  id            : GitHubRepositoryId,
  name          : String,
  owner         : String,
  visibility    : Visibility,
  fork          : Boolean,
  size          : Long,
  cloneURL      : String,
  defaultBranch : String,
  branches      : Seq[Branch]
) extends Repository {
  val desc     = RepositoryDesc.fromGitHub(owner, name)
  val remote   = Some(Remote("origin", cloneURL))
  val fullName = s"$owner/$name"
}

case class TequilaRepository(
  sciper        : String,
  name          : String,
  defaultBranch : String,
  branches      : Seq[Branch],
  remote        : Option[Remote] = None
) extends Repository {
  val desc       = RepositoryDesc.fromTequila(sciper, name)
  val visibility = Visibility.Private
  val fork       = false
  val fullName   = name
}

case class LocalRepository(
  name          : String,
  path          : String,
  defaultBranch : String,
  branches      : Seq[Branch],
  remote        : Option[Remote] = None
) extends Repository {
  val desc       = RepositoryDesc.fromLocal(path)
  val visibility = Visibility.Private
  val fork       = false
  val fullName   = name
}

