/* Copyright 2009-2016 EPFL, Lausanne */

package leon.web
package auth
package providers

import scala.concurrent.Future

import securesocial.core._
import securesocial.core.services.{ CacheService, RoutesService }

/**
  * Authentication provider for Tequila, EPFL's federated
  * identity management service.
  */
class TequilaProvider(routesService: RoutesService, cacheService: CacheService, client: OAuth2Client)
  extends OAuth2Provider(routesService, client, cacheService) {

  override val id = TequilaProvider.Tequila

  private val Error     = "error"
  private val FirstName = "Firstname"
  private val LastName  = "Name"
  private val Email     = "Email"
  private val Sciper    = "Sciper"
  private val Username  = "Username"

  override def fillProfile(info: OAuth2Info): Future[BasicProfile] = {
    val profileUrl = TequilaProvider.GetProfileApi(info.accessToken)

    client.retrieveProfile(profileUrl) map { json =>
      (json \ Error).asOpt[String] match {
        case Some(msg) =>
          logger.error(s"[securesocial] error retrieving profile information from Tequila. Message = $msg")
          throw new AuthenticationException()

        case None =>
          val userId    = (json \ Sciper).as[Int]
          val firstName = (json \ FirstName).asOpt[String]
          val lastName  = (json \ LastName).asOpt[String]
          val email     = (json \ Email).asOpt[String].filter(!_.isEmpty)
          val username  = (json \ Username).asOpt[String]

        BasicProfile(
          providerId   = id,
          userId       = userId.toString,
          firstName    = firstName,
          lastName     = lastName,
          fullName     = None,
          email        = email,
          avatarUrl    = None,
          authMethod   = authMethod,
          oAuth2Info   = Some(info)
        )
      }
    } recover {
      case e: AuthenticationException =>
        throw e

      case e =>
        logger.error("[securesocial] error retrieving profile information from Tequila", e)
        throw new AuthenticationException()
    }
  }

}

object TequilaProvider {

  val Tequila = "tequila"

  private val Root = "https://tequila.epfl.ch/cgi-bin/OAuth2IdP"

  val GetCodeApi = s"$Root/auth?response_type=code"

  def GetProfileApi(token: String) = {
    import play.utils.UriEncoding

    val encodedToken = UriEncoding.encodePathSegment(token, "UTF-8")
    s"$Root/userinfo?access_token=$encodedToken"
  }

}
