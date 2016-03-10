/* Copyright 2009-2015 EPFL, Lausanne */

package leon.web
package services

import play.api.Logger
import play.api.db._
import play.api.Play.current
import securesocial.core._
import securesocial.core.services.{ UserService, SaveMode }
import securesocial.core.providers.MailToken

import leon.web.models.{ User, UserId, ProviderId, Email }
import leon.web.stores.UserStore
import leon.web.utils.Debug

import scala.concurrent.Future

import _root_.java.sql.Connection

/** SecureSocial UserService implementation that stores and
  * retrieves users from the default database.
  */
class DatabaseUserService extends UserServiceBase {

  implicit val conn = DB.getConnection()

  val debug = Debug(Logger("services.UserService"))

  override def find(providerId: String, userId: String): Future[Option[BasicProfile]] = {
    debug((providerId, userId))

    Future.successful {
      UserStore.findByProviderAndId(ProviderId(providerId), UserId(userId)).map(_.toProfile)
    }
  }

  override def findByEmailAndProvider(email: String, providerId: String): Future[Option[BasicProfile]] = {
    debug((email, providerId))

    Future.successful {
      UserStore.findByEmailAndProvider(Email(email), ProviderId(providerId)).map(_.toProfile)
    }
  }

  private def findProfile(p: BasicProfile): Option[((String, String), User)] = {
    debug(p)

    UserStore.findById(UserId(p.userId)).map { user =>
      (p.providerId, p.userId) -> user
    }
  }

  private def updateProfile(profile: BasicProfile, entry: ((String, String), User)): Future[User] = {
    debug((profile, entry))

    Future.successful {
      entry._2
    }
  }

  override def save(profile: BasicProfile, mode: SaveMode): Future[User] = {
    debug((profile, mode))

    val name = profile.email.getOrElse(profile.fullName)

    Future.successful {
      UserStore.save(User.fromProfile(profile))
    }
  }

  override def link(current: User, to: BasicProfile): Future[User] = {
    debug((current, to))

    Future.successful {
      current
    }
  }

}

