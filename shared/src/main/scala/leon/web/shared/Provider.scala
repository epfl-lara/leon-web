package leon.web
package shared

sealed abstract class Provider(val id: String, val name: String) {
  def isUnknown: Boolean = this == UnknownProvider
}

case object GitHubProvider  extends Provider("github", "GitHub")
case object TequilaProvider extends Provider("tequila", "Tequila")
case object UnknownProvider extends Provider("unknown", "Unknown")

object Provider {

  val all: Set[Provider] =
    Set(GitHubProvider, TequilaProvider)

  val map: Map[String, Provider] =
    all.map(p => (p.id, p)).toMap

  def apply(id: String): Provider =
    map.getOrElse(id, UnknownProvider)

}

