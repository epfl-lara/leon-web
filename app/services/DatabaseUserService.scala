/* Copyright 2009-2015 EPFL, Lausanne */

package leon.web
package services

import play.api.Logger
import play.api.db._
import play.api.Play.current
import play.api.libs.concurrent.Execution.Implicits._

import securesocial.core._
import securesocial.core.services.{ UserService, SaveMode }
import securesocial.core.providers.MailToken

import leon.web.models.{User, Identity}
import leon.web.shared.Provider
import leon.web.stores.{UserStore, IdentityStore}
import leon.web.utils.Debug

import scala.concurrent.Future

import _root_.java.sql.Connection

/** SecureSocial UserService implementation that stores and
  * retrieves users from the default database.
  */
class DatabaseUserService extends UserServiceBase {

  import User._
  import Identity._

  implicit val conn = DB.getConnection()

  val debug = Debug(Logger("services.UserService"))

  /**
   * Finds a SocialUser that maches the specified id
   *
   * @param providerId the provider id
   * @param userId the user id
   * @return an optional profile
   */
  override
  def find(providerId: String, userId: String): Future[Option[BasicProfile]] = Future {
    debug((providerId, userId))

    val provider      = Provider(providerId)
    val serviceUserId = ServiceUserId(userId)

    val id = IdentityStore.findByProviderAndServiceUserId(provider, serviceUserId)

    id.map(Identity.toProfile)
  }

  /**
   * Finds a profile by email and provider
   *
   * @param email - the user email
   * @param providerId - the provider id
   * @return an optional profile
   */
  override
  def findByEmailAndProvider(email: String, providerId: String): Future[Option[BasicProfile]] = {
    debug((email, providerId))

    Future.successful {
      None
    }
  }

  /**
   * Saves a profile.  This method gets called when a user logs in, registers or changes his password.
   * This is your chance to save the user information in your backing store.
   *
   * @param profile the user profile
   * @param mode a mode that tells you why the save method was called
   */
  override
  def save(profile: BasicProfile, mode: SaveMode): Future[User] = Future {
    debug((profile, mode))

    val provider      = Provider(profile.providerId)
    val serviceUserId = ServiceUserId(profile.userId)
    val identity      = IdentityStore.findByProviderAndServiceUserId(provider, serviceUserId)
    val user          = identity flatMap { id => UserStore.findById(id.userId) }

    user.getOrElse {
      UserStore.save(User.fromProfile(profile))
    }
  }

  /**
   * Links the current user to another profile
   *
   * @param current The current user instance
   * @param to the profile that needs to be linked to
   */
  override
  def link(current: User, to: BasicProfile): Future[User] = Future {
    debug((current, to))

    val provider      = Provider(to.providerId)
    val serviceUserId = ServiceUserId(to.userId)
    val identity      = Identity.fromProfile(to, Some(current.userId))

    IdentityStore.save(identity)

    current.copy(identities = current.identities + identity)
  }

}

