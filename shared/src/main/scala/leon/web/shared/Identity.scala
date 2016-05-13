package leon.web
package shared

case class ServiceUserId(value: String)
case class Email(value: String)

case class Identity(
  serviceUserId:    ServiceUserId,
  provider:  Provider,
  firstName: Option[String],
  lastName:  Option[String],
  fullName:  Option[String],
  email:     Option[Email],
  avatarUrl: Option[String]
) {
  
  def fullId = s"${provider.id}-${serviceUserId.value}"

  def nameOrEmail: Option[String] = {
    lazy val firstLast =
      firstName
        .zip(lastName)
        .headOption
        .map { case (f, l) => s"$f $l" }

    fullName orElse firstLast orElse email.map(_.value)
  }
}