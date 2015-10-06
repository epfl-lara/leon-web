
package leon.web
package models

import anorm._
import anorm.SqlParser._
import play.api.db._
import play.api.Play.current

import securesocial.core._

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

  def findByProviderAndId(providerId: String, userId: String): Option[User] = {
    val query = SQL"""
      SELECT FROM users
      WHERE user_id = $userId
        AND provider_id = $providerId
      LIMIT 1
      """

      query.as(parser.singleOpt)
  }

  def findById(userId: String): Option[User] = {
    val query = SQL"""
      SELECT FROM users
      WHERE user_id = $userId
      LIMIT 1
      """

      query.as(parser.singleOpt)
  }

  def findByEmailAndProvider(email: String, providerId: String): Option[User] = {
    val query = SQL"""
      SELECT FROM users
      WHERE email = $email
        AND provider_id = $providerId
      LIMIT 1
      """

      query.as(parser.singleOpt)
  }

  def setup(): Unit = {
    SQL"""
      CREATE TABLE IF NOT EXISTS users (
        provider_id VARCHAR(40),
        user_id VARCHAR(40),
        first_name VARCHAR,
        lastName VARCHAR,
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

