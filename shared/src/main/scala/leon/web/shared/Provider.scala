package leon.web
package shared

sealed abstract class Provider(val id: String) {
  def isUnknown: Boolean = this == Provider.Unknown
}

object Provider {

  case object GitHub  extends Provider("github")
  case object Tequila extends Provider("tequila")
  case object Unknown extends Provider("unknown")

  val all = Seq(GitHub, Tequila)

  val map = all.map(p => (p.id, p)).toMap

  def apply(id: String): Provider =
    map.getOrElse(id, Unknown)

}

