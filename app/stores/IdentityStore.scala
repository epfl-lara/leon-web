/* Copyright 2009-2016 EPFL, Lausanne */

package leon.web
package stores

import anorm._
import anorm.SqlParser._
import java.sql.Connection
import play.api.Play.current
import leon.web.models.Identity
import leon.web.shared.Provider
import securesocial.core._
import shared.{Identity => SharedIdentity, _}

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
      firstName     <- str("first_name").?
      lastName      <- str("last_name").?
      fullName      <- str("full_name").?
      email         <- str("email").?
      avatarUrl     <- str("avatar_url").?
      authMethod    <- str("auth_method")
      accessToken   <- str("access_token").?
    }
    yield Identity(
      i= SharedIdentity(
        ServiceUserId(serviceUserId),
        Provider(providerId),
        firstName, lastName, fullName,
        email.map(Email), avatarUrl),
      userId=     UserId(userId),
      authMethod= AuthenticationMethod(authMethod),
      oAuth2Info= accessToken.map(OAuth2Info(_, None, None, None))
    )
  }

  def findByUserId(id: UserId)(implicit c: Connection): Set[Identity] = {
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
      user_id, provider_id, service_user_id,
      first_name, last_name, full_name,
      email, avatar_url,
      auth_method, access_token
    )
    VALUES (
      ${i.userId.value}, ${i.i.provider.id}, ${i.i.serviceUserId.value},
      ${i.i.firstName}, ${i.i.lastName}, ${i.i.fullName},
      ${i.i.email.map(_.value)}, ${i.i.avatarUrl},
      ${i.authMethod.method}, ${i.oAuth2Info.map(_.accessToken)}
    )
    """

    query.executeInsert()

    i
  }

  def delete(i: Identity)(implicit c: Connection): Boolean = {
    val query = SQL"""
      DELETE FROM identities
      WHERE user_id         = ${i.userId.value}
        AND provider_id     = ${i.i.provider.id}
        AND service_user_id = ${i.i.serviceUserId.value}
      LIMIT 1
    """

    query.executeUpdate() > 0
  }

}

