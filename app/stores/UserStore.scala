
package leon.web
package stores

import anorm._
import anorm.SqlParser._
import java.sql.Connection
import play.api.db._
import play.api.Play.current

import leon.web.models.{User, UserId, ProviderId, Email}

import securesocial.core._

object UserStore {

  def parser = {
    for {
      providerId  <- str("provider_id")
      userId      <- str("user_id")
      firstName   <- str("first_name").?
      lastName    <- str("last_name").?
      fullName    <- str("full_name").?
      email       <- str("email").?
      avatarUrl   <- str("avatar_url").?
      authMethod  <- str("auth_method")
      accessToken <- str("access_token").?
    }
    yield User(
      ProviderId(providerId), UserId(userId),
      firstName, lastName, fullName,
      email.map(Email), avatarUrl,
      AuthenticationMethod(authMethod),
      accessToken.map(OAuth2Info(_, None, None, None))
    )
  }

  def findByProviderAndId(providerId: ProviderId, userId: UserId)
                         (implicit c: Connection): Option[User] = {
    val query = SQL"""
      SELECT * FROM users
      WHERE user_id = ${userId.value}
        AND provider_id = ${providerId.value}
      LIMIT 1
      """

      query.as(parser.singleOpt)
  }

  def findById(userId: UserId)(implicit c: Connection): Option[User] = {
    val query = SQL"""
      SELECT * FROM users
      WHERE user_id = ${userId.value}
      LIMIT 1
      """

      query.as(parser.singleOpt)
  }

  def findByEmailAndProvider(email: Email, providerId: ProviderId)
                            (implicit c: Connection): Option[User] = {
    val query = SQL"""
      SELECT * FROM users
      WHERE email = ${email.value}
        AND provider_id = ${providerId.value}
      LIMIT 1
      """

      query.as(parser.singleOpt)
  }

  def save(u: User)(implicit c: Connection): User = {
    val query = SQL"""
    MERGE INTO users (provider_id, user_id,
                       first_name, last_name, full_name,
                       email, avatar_url,
                       auth_method, access_token)
    VALUES (${u.providerId.value}, ${u.userId.value},
            ${u.firstName}, ${u.lastName}, ${u.fullName},
            ${u.email.map(_.value)}, ${u.avatarUrl},
            ${u.authMethod.method}, ${u.oAuth2Info.map(_.accessToken)})
    """

    query.executeInsert()

    findByProviderAndId(u.providerId, u.userId).get
  }

}

