
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

import securesocial.core._
import securesocial.core.services.{ UserService, SaveMode }
import securesocial.core.providers.MailToken

import leon.web.models.User

import scala.concurrent.Future

abstract class UserServiceBase extends UserService[User] {

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

