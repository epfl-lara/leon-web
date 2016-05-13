package leon.web
package shared

sealed abstract class Provider {
  def id: String
  def isUnknown: Boolean = this == UnknownProvider
}

case object GitHubProvider  extends Provider { val id = "github" }
case object TequilaProvider extends Provider { val id = "tequila" }
case object UnknownProvider extends Provider { val id = "unknown" }

object Provider {

  val all: Set[Provider] =
    Set(GitHubProvider, TequilaProvider)

  val map: Map[String, Provider] =
    all.map(p => (p.id, p)).toMap

  def apply(id: String): Provider =
    map.getOrElse(id, UnknownProvider)

}

