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
import securesocial.core.services.{ UserService => SSUserService, SaveMode }
import securesocial.core.providers.MailToken

import leon.web.models.User

import scala.concurrent.Future

class UserService extends SSUserService[User] {

  val logger = Logger("services.UserService")

  var users = Map[(String, String), User]()

  override def find(providerId: String, userId: String): Future[Option[BasicProfile]] = {
    if (logger.isDebugEnabled) {
      logger.debug("users = %s".format(users))
    }

    val result = for (
      user <- users.values;
      basicProfile <- user.identities.find(su => su.providerId == providerId && su.userId == userId)
    ) yield basicProfile

    Future.successful(result.headOption)
  }

  override def findByEmailAndProvider(email: String, providerId: String): Future[Option[BasicProfile]] = {
    if (logger.isDebugEnabled) {
      logger.debug("users = %s".format(users))
    }

    val someEmail = Some(email)
    val result = for (
      user <- users.values;
      basicProfile <- user.identities.find(su => su.providerId == providerId && su.email == someEmail)
    ) yield basicProfile

    Future.successful(result.headOption)
  }

  private def findProfile(p: BasicProfile) = {
    users.find {
      case (key, value) if value.identities.exists(su => su.providerId == p.providerId && su.userId == p.userId) => true
      case _ => false
    }
  }

  private def updateProfile(user: BasicProfile, entry: ((String, String), User)): Future[User] = {
    val identities = entry._2.identities
    val updatedList = identities.patch(identities.indexWhere(i => i.providerId == user.providerId && i.userId == user.userId), Seq(user), 1)
    val updatedUser = entry._2.copy(identities = updatedList)
    users = users + (entry._1 -> updatedUser)
    Future.successful(updatedUser)
  }

  override def save(user: BasicProfile, mode: SaveMode): Future[User] = {
    mode match {
      case SaveMode.SignUp =>
        val newUser = User(user, List(user))
        users = users + ((user.providerId, user.userId) -> newUser)
        Future.successful(newUser)

      case SaveMode.LoggedIn =>
        findProfile(user) match {
          case Some(existingUser) =>
            updateProfile(user, existingUser)

          case None =>
            val newUser = User(user, List(user))
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
    if (current.identities.exists(i => i.providerId == to.providerId && i.userId == to.userId)) {
      Future.successful(current)
    } else {
      val added = to :: current.identities
      val updatedUser = current.copy(identities = added)
      users = users + ((current.main.providerId, current.main.userId) -> updatedUser)
      Future.successful(updatedUser)
    }
  }

  override def saveToken(token: MailToken): Future[MailToken] = {
    throw new UnsupportedOperationException("UserService.saveToken should never be called")
  }

  override def findToken(token: String): Future[Option[MailToken]] = {
    throw new UnsupportedOperationException("UserService.findToken should never be called")
  }

  override def deleteToken(uuid: String): Future[Option[MailToken]] = {
    throw new UnsupportedOperationException("UserService.deleteToken should never be called")
  }

  override def deleteExpiredTokens(): Unit = {
    throw new UnsupportedOperationException("UserService.deleteExpiredTokens should never be called")
  }

  override def updatePasswordInfo(user: User, info: PasswordInfo): Future[Option[BasicProfile]] = {
    throw new UnsupportedOperationException("UserService.updatePasswordInfo should never be called")
  }

  override def passwordInfoFor(user: User): Future[Option[PasswordInfo]] = {
    throw new UnsupportedOperationException("UserService.passwordInfoFor should never be called")
  }

}

