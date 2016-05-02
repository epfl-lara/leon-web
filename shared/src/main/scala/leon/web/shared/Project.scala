package leon.web
package shared

case class Project(
  repo: RepositoryDesc,
  branch: String,
  file: String,
  code: Option[String] = None
)

