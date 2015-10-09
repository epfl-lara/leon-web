
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

