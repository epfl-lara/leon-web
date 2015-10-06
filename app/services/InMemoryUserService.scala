/**
 * Copyright 2012 Jorge Aliss (jaliss at gmail dot com) - twitter: @jaliss
 * Copyright 2015 Romain Ruetschi (romain.ruetschi at epfl dot com) - twitter: @_romac
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */

package leon.web
package services

import play.api.Logger
import securesocial.core._
import securesocial.core.services.{ UserService, SaveMode }
import securesocial.core.providers.MailToken

import leon.web.models.User

import scala.concurrent.Future

class InMemoryUserService extends UserServiceBase {

  val logger = Logger("services.InMemoryUserService")

  var users = Map[(String, String), User]()

  private def by(providerId: String)(p: User => Boolean)(user: User): Boolean =
    user.profile.providerId == providerId && p(user)

  override def find(providerId: String, userId: String): Future[Option[BasicProfile]] = {
    if (logger.isDebugEnabled) {
      logger.debug("find(%s, %s)".format(providerId, userId))
    }

    val result =
      users.values
        .filter(by(providerId)(_.profile.userId == userId))
        .map(_.profile)

    Future.successful(result.headOption)
  }

  override def findByEmailAndProvider(email: String, providerId: String): Future[Option[BasicProfile]] = {
    if (logger.isDebugEnabled) {
      logger.debug("findByEmailAndProvider(%s, %s)".format(email, providerId))
    }

    val someEmail = Some(email)
    val result =
      users.values
        .filter(by(providerId)(_.profile.email == email))
        .map(_.profile)

    Future.successful(result.headOption)
  }

  private def findProfile(p: BasicProfile): Option[((String, String), User)] = {
    if (logger.isDebugEnabled) {
      logger.debug("findProfile(%s)".format(p))
    }

    users.values
      .find(_.profile.userId == p.userId)
      .map { (p.providerId, p.userId) -> _ }
  }

  private def updateProfile(profile: BasicProfile, entry: ((String, String), User)): Future[User] = {
    if (logger.isDebugEnabled) {
      logger.debug("updateProfile(%s, %s)".format(profile, entry))
    }

    val (key, user) = entry
    val updatedUser = user.copy(profile = profile)
    users = users + (key -> updatedUser)
    Future.successful(updatedUser)
  }

  override def save(user: BasicProfile, mode: SaveMode): Future[User] = {
    if (logger.isDebugEnabled) {
      logger.debug("save(%s, %s)".format(user, mode))
    }

    mode match {
      case SaveMode.SignUp =>
        val newUser = User(user)
        users = users + ((user.providerId, user.userId) -> newUser)
        Future.successful(newUser)

      case SaveMode.LoggedIn =>
        findProfile(user) match {
          case Some(existingUser) =>
            updateProfile(user, existingUser)

          case None =>
            val newUser = User(user)
            users = users + ((user.providerId, user.userId) -> newUser)
            Future.successful(newUser)
        }

      case SaveMode.PasswordChange =>
        findProfile(user).map { entry => updateProfile(user, entry) }.getOrElse {
          throw new Exception(s"Missing profile for user ${user.userId}")
        }
    }
  }

  override def link(current: User, to: BasicProfile): Future[User] = {
    if (logger.isDebugEnabled) {
      logger.debug("link(%s, %s)".format(current, to))
    }

    val profile = current.profile
    if (profile.providerId == to.providerId && profile.userId == to.userId) {
      Future.successful(current)
    } else {
      val updatedUser = current.copy(profile = to)
      users = users + ((profile.providerId, profile.userId) -> updatedUser)
      Future.successful(updatedUser)
    }
  }

}

