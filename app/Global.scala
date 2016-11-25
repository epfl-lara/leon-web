
import java.lang.reflect.Constructor
import scala.collection.immutable.ListMap

import play.api._
import play.api.data.Form
import play.api.i18n.Lang
import play.twirl.api.Html
import play.api.mvc.RequestHeader

import securesocial.core.RuntimeEnvironment
import securesocial.core.authenticator.{CookieAuthenticatorBuilder, HttpHeaderAuthenticatorBuilder}
import securesocial.core.services.AuthenticatorService
import securesocial.core.providers.GitHubProvider

import leon.web.models.User
import leon.web.services.DatabaseUserService
import leon.web.stores.CookieAuthenticatorStore
import leon.web.config.RepositoryService
import leon.web.auth.providers.TequilaProvider

object Global extends GlobalSettings {

  override def onStart(app: Application) = {
    Logger.info("Application has started")

    checkConfiguration(app)
  }

  def checkConfiguration(app: Application) = {
    Logger.info("Checking configuration...")

    try {
      val config = RepositoryService.Config.fromPlayAppConfig(app.configuration)
      Logger.info("√ Configuration is OK!")
    }
    catch {
      case e: Exception =>
        Logger.error(s"X Configuration is INVALID: $e")
        throw e
    }
  }

  object RuntimeEnv extends RuntimeEnvironment.Default[User] {

    override lazy val userService = new DatabaseUserService

    override lazy val authenticatorService = new AuthenticatorService(
      new CookieAuthenticatorBuilder(new CookieAuthenticatorStore, idGenerator)
    )

    override lazy val providers = ListMap(
      include(new GitHubProvider(routes, cacheService, oauth2ClientFor(GitHubProvider.GitHub))),
      include(new TequilaProvider(routes, cacheService, oauth2ClientFor(TequilaProvider.Tequila)))
    )
 }

  /**
    * Inject RuntimeEnv into controllers requiring it.
    */
  override def getControllerInstance[A](controllerClass: Class[A]): A = {
    val instance = controllerClass.getConstructors.find { c =>
      val params = c.getParameterTypes
      params.length == 1 && params(0) == classOf[RuntimeEnvironment[User]]
    }.map {
      _.asInstanceOf[Constructor[A]].newInstance(RuntimeEnv)
    }
    instance.getOrElse(super.getControllerInstance(controllerClass))
  }

}

