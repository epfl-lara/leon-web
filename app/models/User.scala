
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

object User {

  implicit val conn = DB.getConnection()

  def parser = {
    implicit def toSome[A](a: A): Option[A] = Some(a)

    for {
      providerId  <- str("provider_id")
      userId      <- str("user_id")
      firstName   <- str("first_name")
      lastName    <- str("last_name")
      fullName    <- str("full_name")
      email       <- str("email")
      avatarUrl   <- str("avatar_url")
      authMethod  <- str("auth_method")
      accessToken <- str("access_token")
    }
    yield User(BasicProfile(
      providerId, userId,
      firstName, lastName, fullName,
      email, avatarUrl,
      AuthenticationMethod(authMethod),
      None,
      OAuth2Info(accessToken),
      None
    ))
  }

  def findByProviderAndId(providerId: ProviderId, userId: UserId): Option[User] = {
    val query = SQL"""
      SELECT * FROM users
      WHERE user_id = ${userId.value}
        AND provider_id = ${providerId.value}
      LIMIT 1
      """

      query.as(parser.singleOpt)
  }

  def findById(userId: UserId): Option[User] = {
    val query = SQL"""
      SELECT * FROM users
      WHERE user_id = ${userId.value}
      LIMIT 1
      """

      query.as(parser.singleOpt)
  }

  def findByEmailAndProvider(email: Email, providerId: ProviderId): Option[User] = {
    val query = SQL"""
      SELECT * FROM users
      WHERE email = ${email.value}
        AND provider_id = ${providerId.value}
      LIMIT 1
      """

      query.as(parser.singleOpt)
  }

  def save(user: User): User = {
    val p = user.profile
    val query = SQL"""
    INSERT INTO users (provider_id, user_id,
                       first_name, last_name, full_name,
                       email, avatar_url,
                       auth_method, access_token)
    VALUES (${p.providerId}, ${p.userId}, ${p.firstName}, ${p.lastName}, ${p.email}, ${p.avatarUrl}, ${p.authMethod.method}, ${p.oAuth2Info.map(_.accessToken)})
    """

    query.executeInsert()

    user
  }

  def setup(): Unit = {
    SQL"""
      CREATE TABLE IF NOT EXISTS users (
        provider_id VARCHAR(40),
        user_id VARCHAR(40),
        first_name VARCHAR,
        last_name VARCHAR,
        full_name VARCHAR,
        email VARCHAR,
        avatar_url VARCHAR,
        auth_method VARCHAR,
        access_token VARCHAR,

        PRIMARY KEY (provider_id, user_id)
      )
    """.execute
  }

}

