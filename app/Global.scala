
import java.lang.reflect.Constructor
import scala.collection.immutable.ListMap

import play.api._
import play.api.data.Form
import play.api.i18n.Lang
import play.twirl.api.Html
import play.api.mvc.RequestHeader
import securesocial.core.RuntimeEnvironment
import securesocial.core.providers.GitHubProvider
import securesocial.controllers.{ViewTemplates => SSViewTemplates}

import leon.web.models.{Permalink, User}
import leon.web.services._

object Global extends GlobalSettings {
  override def onStart(app: Application) {
    Permalink.setup()
  }

  class ViewTemplates(env: RuntimeEnvironment[_]) extends SSViewTemplates.Default(env) {
    override def getLoginPage(form: Form[(String, String)], msg: Option[String] = None)
                             (implicit request: RequestHeader, lang: Lang): Html = {
      views.html.login(form, msg)
    }
  }

  object RuntimeEnv extends RuntimeEnvironment.Default[User] {
    override lazy val userService = new InMemoryUserService
    // override lazy val viewTemplates: ViewTemplates = new ViewTemplates(this)
    override lazy val providers = ListMap(
      include(new GitHubProvider(routes, cacheService, oauth2ClientFor(GitHubProvider.GitHub)))
    )
  }

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

