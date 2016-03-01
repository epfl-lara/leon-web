package leon.web
package shared

case class Project(
  owner: String,
  repo: String,
  branch: String,
  file: String,
  code: Option[String] = None
)

