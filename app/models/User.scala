
package leon.web
package models

import anorm._
import anorm.SqlParser._
import play.api.db._
import play.api.Play.current

import securesocial.core._

case class UserId(value: String) extends AnyVal
case class ProviderId(value: String) extends AnyVal
case class Email(value: String) extends AnyVal

case class User(profile: BasicProfile)

