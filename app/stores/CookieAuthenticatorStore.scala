package leon.web
package stores

import scala.reflect.ClassTag
import scala.concurrent.{ ExecutionContext, Future }

import play.api.db._
import play.api.Play.current
import play.api.Logger

import securesocial.core._
import securesocial.core.authenticator._

import anorm._
import anorm.SqlParser._

import org.joda.time.DateTime

import _root_.java.sql.Connection

import leon.web.models.{User, UserId}

class CookieAuthenticatorStore(implicit executionContext: ExecutionContext)
  extends AuthenticatorStore[CookieAuthenticator[User]] {

  type Authenticator = CookieAuthenticator[User]

  implicit val conn = DB.getConnection()

  def parser = {
    for {
      authId         <- str("id")
      userId         <- str("user_id")
      expirationDate <- get[DateTime]("expiration_date")
      lastUsed       <- get[DateTime]("last_used")
      creationDate   <- get[DateTime]("creation_date")
    } yield CookieAuthenticator(
      authId,
      UserId(userId),
      expirationDate,
      lastUsed,
      creationDate,
      this.asInstanceOf[AuthenticatorStore[CookieAuthenticator[UserId]]]
    )
  }

  /**
   * Retrieves an Authenticator from the backing store
   *
   * @param id the authenticator id
   * @param ct the class tag for the Authenticator type
   * @return an optional future Authenticator
   */
  override
  def find(id: String)(implicit ct: ClassTag[Authenticator]): Future[Option[Authenticator]] = {
    val now = new DateTime()

    val query = SQL"""
      SELECT * FROM cookie_authenticators
      WHERE id = $id AND expiration_date > $now
      LIMIT 1
      """

    Future {
      for {
        auth <- query.as(parser.singleOpt)
        user <- UserStore.findById(auth.user)
      } yield CookieAuthenticator(
        auth.id,
        user,
        auth.expirationDate,
        auth.lastUsed,
        auth.creationDate,
        this
      )
    }
  }

  /**
   * Saves/updates an authenticator in the backing store
   *
   * @param authenticator the istance to save
   * @param timeoutInSeconds the timeout. after this time has passed the backing store needs to remove the entry.
   * @return the saved authenticator
   */
  override
  def save(auth: Authenticator, timeoutInSeconds: Int): Future[Authenticator] = {
    val query = SQL"""
      MERGE INTO cookie_authenticators
        (id, user_id, expiration_date, last_used, creation_date)
        VALUES (${auth.id}, ${auth.user.userId.value}, ${auth.expirationDate}, ${auth.lastUsed}, ${auth.creationDate})
      """

      Future {
        query.executeInsert()
        auth
      }
  }

  /**
   * Deletes an Authenticator from the backing store
   *
   * @param id the authenticator id
   * @return a future of Unit
   */
  override
  def delete(id: String): Future[Unit] = {
    val query = SQL"""DELETE FROM cookie_authenticators WHERE id = $id LIMIT 1"""

    Future {
      query.executeUpdate()
    }
  }

}
