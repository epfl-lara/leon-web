/* Copyright 2009-2015 EPFL, Lausanne */

package leon.web
package stores

import anorm._
import anorm.SqlParser._
import java.sql.Connection
import play.api.Play.current
import leon.web.models._
import leon.web.shared.{Provider, UserId}
import securesocial.core._

/** Provides methods to retrieve and store a [[leon.web.models.User]]
  * to/from the database.
  *
  * @see [[leon.web.services.DatabaseUserService]]
  */
object UserStore {

  import User._

  def parser = {
    for {
      userId         <- str("user_id")
      mainProviderId <- str("main_provider_id")
    }
    yield (UserId(userId), Provider(mainProviderId))
  }

  def findById(userId: UserId)(implicit c: Connection): Option[User] = {
    val query = SQL"""
      SELECT * FROM users
      WHERE user_id = ${userId.value}
      LIMIT 1
      """

    query.as(parser.singleOpt) map { case (userId, provider) =>
      val identities = IdentityStore.findByUserId(userId)
      User(userId, provider, identities)
    }
  }

  def save(u: User)(implicit c: Connection): User = {
    val query = SQL"""
      MERGE INTO users (user_id, main_provider_id)
      VALUES (${u.userId.value}, ${u.main.publicId.provider.id})
    """

    query.executeInsert()

    u.identities.foreach(IdentityStore.save)

    findById(u.userId).get
  }

  def unlinkIdentity(user: User, id: Identity)(implicit c: Connection): User = {
    if (IdentityStore delete id)
      save(user unlink id)
    else
      user
  }

}

