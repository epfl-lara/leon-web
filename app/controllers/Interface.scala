package leon.web
package controllers

import play.api._
import play.api.mvc._
import play.api.libs.iteratee._
import play.api.libs.concurrent._
import play.api.libs.json._
import play.api.libs.json.Json._
import play.api.libs.json.Writes._

import securesocial.core._

import models.FileExamples
import models.ConsoleProtocol._
import models.LeonWebConfig
import models.User

import akka.actor._
import scala.concurrent.duration._
import scala.concurrent.Future
import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global

import akka.util.Timeout
import akka.pattern.ask

class Interface(override implicit val env: RuntimeEnvironment[User]) extends SecureSocial[User] {

  def getExamples(dir: String) = {
    List(
      dir -> new FileExamples(dir).allExamples
    )
  }


  def index(dir: String) = UserAwareAction { implicit request =>
    val examples = if (dir == "") {
      getExamples("verification") ++ getExamples("synthesis") ++ getExamples("invariant")
    } else {
      getExamples(dir)
    }

    LeonWebConfig.fromCurrent(examples) match {
      case Some(webconfig) =>
        Ok(views.html.index(webconfig, request.user))

      case None =>
        Redirect(routes.Interface.index("")).flashing {
          "error" -> s"Page '$dir' not found"
        }
    }

  }

  def getExample(kind: String, id: Int) = Action { 
    getExamples(kind).headOption.flatMap(_._2.lift.apply(id)) match {
      case Some(ex) =>
        Ok(toJson(Map("status" -> "success", "code" -> ex.code)))
      case None =>
        Ok(toJson(Map("status" -> "error", "errormsg" -> "Unknown example")))
    }
  }

  def openConsole() = WebSocket.tryAccept[JsValue] { request =>
    import play.api.Play.current

    val session = Akka.system.actorOf(Props(new models.ConsoleSession(request.remoteAddress)))
    implicit val timeout = Timeout(1.seconds)

    (session ? Init).map {
      case InitSuccess(enumerator) =>
        // Create an Iteratee to consume the feed
        val iteratee = Iteratee.foreach[JsValue] { event =>
          session ! ProcessClientEvent(event)
        }.map { _ =>
          session ! Quit
        }

        Right((iteratee,enumerator))

      case InitFailure(error: String) =>
        // Connection error

        // A finished Iteratee sending EOF
        val iteratee = Done[JsValue,Unit]((),Input.EOF)

        // Send an error and close the socket
        val enumerator =  Enumerator[JsValue](toJson(
          Map("kind" -> "error", "message" -> error)
        )).andThen(Enumerator.enumInput(Input.EOF))

        Right((iteratee,enumerator))
    }
  }

  def login = Action {
    Redirect(securesocial.controllers.routes.ProviderController.authenticate("github", None))
  }

}
