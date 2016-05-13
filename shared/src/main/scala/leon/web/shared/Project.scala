package leon.web
package shared

case class Project(
  repo: Repository,
  branch: String,
  file: String,
  code: Option[String] = None
)

