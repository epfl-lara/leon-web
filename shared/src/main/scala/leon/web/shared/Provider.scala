package leon.web
package shared

sealed abstract class Provider(val id: String, val name: String) {

  def isUnknown: Boolean = this == Provider.Unknown
  def isKnown: Boolean   = !isUnknown

  override
  def toString = id
}


object Provider {

  case object GitHub  extends Provider("github",  "GitHub")
  case object Tequila extends Provider("tequila", "Tequila")
  case object Local   extends Provider("local",   "Local")
  case object Unknown extends Provider("unknown", "Unknown")

  val all: List[Provider] =
    List(GitHub, Tequila)

  val map: Map[String, Provider] =
    all.map(p => (p.id, p)).toMap

  def apply(id: String): Provider =
    map.getOrElse(id, Unknown)

}

