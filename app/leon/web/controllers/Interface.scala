package leon.web
package controllers

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.TimeoutException
import akka.actor._
import akka.pattern.ask
import akka.util.Timeout
import models.ConsoleProtocol._
import models.FileExamples
import models.LeonWebConfig
import models.User
import config.RepositoryService.{Config => RepositoryServiceConfig}
import services.RepositoryService
import play.api.libs.concurrent._
import play.api.libs.iteratee._
import play.api.libs.json._
import play.api.libs.json.Json._
import play.api.libs.json.Writes._
import play.api.mvc._
import securesocial.core._
import shared.messages.{MessageFromServer, HLog}
import play.api.Play

class Interface(override implicit val env: RuntimeEnvironment[User]) extends SecureSocial[User] {

  def getExamples(dir: String) = {
    List(
      dir -> new FileExamples(dir).allExamples
    )
  }
  
  def preview = UserAwareAction { implicit request => 
    val url = Play.current.configuration.getString("app.url").getOrElse("/")
    val ssl = Play.current.configuration.getBoolean("app.ssl").getOrElse(true)
    val webconfig = LeonWebConfig(
      examples=Nil,
      default=null,
      url=url,
      isSSL=ssl,
      release=LeonWebConfig.getLeonRelease
    )

    Ok(views.html.preview(webconfig))
  }


  def index(dir: String) = UserAwareAction { implicit request =>
    val examples = if (dir === "") {
      getExamples("verification") ++ getExamples("synthesis") ++ getExamples("resourcebounds") ++ getExamples("memresources")
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

  def openConsole() = WebSocket.tryAccept[Array[Byte]] { implicit request =>
    import play.api.Play.current

    val userFuture = SecureSocial.currentUser.recover {
       case t: TimeoutException => None
     }
    import scala.concurrent.duration._
    val user    = Await.result(userFuture, 1.seconds)
    val session = Akka.system.actorOf(Props(new models.ConsoleSession(request.remoteAddress, user)))
    implicit val timeout: Timeout = Timeout(1.seconds)

    /*val out = Enumerator[Array[Byte]]()

    val in = Iteratee.foreach[Array[Byte]](content => {
  	   session ! ProcessClientEvent(content)
    }).map{ _ => session ! Quit }*/

    (session ? Init).map {
      case InitSuccess(enumerator) =>
        // Create an Iteratee to consume the feed
        val iteratee = Iteratee.foreach[Array[Byte]] { event =>
          session ! ProcessClientEvent(event)
        }.map { _ =>
          session ! Quit
        }

        Right((iteratee,enumerator))

      case InitFailure(error: String) =>
        // Connection error

        // A finished Iteratee sending EOF
        val iteratee = Done[Array[Byte],Unit]((),Input.EOF)

        import boopickle.Default._
        import shared.messages.MessageFromServer._
        // Send an error and close the socket
        val enumerator =  Enumerator[Array[Byte]](Pickle.intoBytes[MessageFromServer](HLog(error)).array()).andThen(Enumerator.enumInput(Input.EOF))

        Right((iteratee,enumerator))
    }
  }

}
