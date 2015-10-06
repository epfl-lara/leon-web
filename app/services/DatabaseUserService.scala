package leon.web
package services

import play.api.Logger
import securesocial.core._
import securesocial.core.services.{ UserService, SaveMode }
import securesocial.core.providers.MailToken

import leon.web.models.User

import scala.concurrent.Future

class DatabaseUserService extends UserServiceBase {

  val logger = Logger("services.DatabaseUserService")

  override def find(providerId: String, userId: String): Future[Option[BasicProfile]] = {
    Future.successful {
      User.findByProviderAndId(providerId, userId).map(_.profile)
    }
  }

  override def findByEmailAndProvider(email: String, providerId: String): Future[Option[BasicProfile]] = {
    Future.successful {
      User.findByEmailAndProvider(email, providerId).map(_.profile)
    }
  }

  private def findProfile(p: BasicProfile): Option[((String, String), User)] = {
    User.findById(p.userId).map { user =>
      (p.providerId, p.userId) -> user
    }
  }

  private def updateProfile(profile: BasicProfile, entry: ((String, String), User)): Future[User] = {
    Future.successful {
      entry._2
    }
  }

  override def save(user: BasicProfile, mode: SaveMode): Future[User] = {
    Future.successful {
      User.save(User(user))
    }
  }

  override def link(current: User, to: BasicProfile): Future[User] = {
    Future.successful {
      current
    }
  }

}

