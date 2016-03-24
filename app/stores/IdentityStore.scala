/* Copyright 2009-2016 EPFL, Lausanne */

package leon.web
package stores

import anorm._
import anorm.SqlParser._
import java.sql.Connection
import play.api.Play.current

import leon.web.models.{Identity, User}
import leon.web.shared.Provider

import securesocial.core._

/**
  * Provides methods to retrieve and store an [[leon.web.models.Identity]]
  * to/from the database.
  */
object IdentityStore {

  import Identity._

  def parser = {
    for {
      userId        <- str("user_id")
      providerId    <- str("provider_id")
      serviceUserId <- str("service_user_id")
    }
    yield Identity(
      User.UserId(userId),
      Provider(providerId),
      ServiceUserId(serviceUserId)
    )
  }

  def findByUserId(id: User.UserId)(implicit c: Connection): Set[Identity] = {
    val query = SQL"""
      SELECT * FROM identities
      WHERE user_id = ${id.value}
      """

    query.as(parser.*).toSet
  }

  def findByServiceUserId(id: ServiceUserId)(implicit c: Connection): Option[Identity] = {
    val query = SQL"""
      SELECT * FROM identities
      WHERE service_user_id = ${id.value}
      LIMIT 1
      """

    query.as(parser.singleOpt)
  }

  def findByProviderAndServiceUserId(provider: Provider, id: ServiceUserId)
                                    (implicit c: Connection): Option[Identity] = {
    val query = SQL"""
      SELECT * FROM identities
      WHERE provider_id = ${provider.id} AND service_user_id = ${id.value}
      LIMIT 1
      """

    query.as(parser.singleOpt)
  }

  def save(i: Identity)(implicit c: Connection): Identity = {
    val query = SQL"""
    MERGE INTO identities (
      user_id, provider_id, service_user_id
    )
    VALUES (
      ${i.userId.value}, ${i.provider.id}, ${i.serviceUserId.value}
    )
    """

    query.executeInsert()

    i
  }

}

