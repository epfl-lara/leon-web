package leon.web
package shared

case class RepositoryState(
  repo: Repository,
  branch: String,
  file: String,
  asProject: Boolean,
  code: Option[String] = None
) {

  def asString: String = s"$repo:$branch:$file:$asProject"

}

