package leon.web
package controllers

import play.api._
import play.api.mvc._
import play.api.libs.iteratee._
import play.api.libs.concurrent._
import play.api.libs.json._
import play.api.libs.json.Json._
import play.api.libs.json.Writes._

import models.FileExamples
import models.ConsoleProtocol._

import akka.actor._
import scala.concurrent.duration._
import scala.concurrent.Future
import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global

import akka.util.Timeout
import akka.pattern.ask

object Interface extends Controller {

  val normalExamples = List(
    "Verification" -> new FileExamples("verification").allExamples,
    "Synthesis"    -> new FileExamples("synthesis").allExamples
  )

  val tutorialExamples = List(
    "Tutorials"    -> new FileExamples("tutorials").allExamples
  )

  val demosExamples = List(
    "Demos"    -> new FileExamples("demos").allExamples
  )

  val allExamples = normalExamples ++ tutorialExamples ++ demosExamples

  def getLeonRelease: String = {
    import java.io.File
    import scala.io.Source

    val f = new File("./version")
    if (f.isFile) {
      Source.fromFile(f).getLines.toList.headOption.getOrElse("")
    } else {
      ""
    }
  }

  def index() = Action { implicit request =>
    val prefix = Play.current.configuration.getString("app.prefix").getOrElse("")
    val url    = Play.current.configuration.getString("app.url").getOrElse("/")

    Ok(views.html.index(normalExamples, normalExamples.tail.head._2(1), prefix, url, getLeonRelease))
  }

  def tutorials() = Action { implicit request =>
    val prefix = Play.current.configuration.getString("app.prefix").getOrElse("")
    val url    = Play.current.configuration.getString("app.url").getOrElse("/")

    Ok(views.html.index(tutorialExamples, tutorialExamples.head._2.head, prefix, url, getLeonRelease))
  }

  def demos() = Action { implicit request =>
    val prefix = Play.current.configuration.getString("app.prefix").getOrElse("")
    val url    = Play.current.configuration.getString("app.url").getOrElse("/")

    Ok(views.html.index(demosExamples, demosExamples.head._2.head, prefix, url, getLeonRelease))
  }

  def getExample(kind: String, id: Int) = Action { 
    allExamples.toMap.get(kind).flatMap(_.lift.apply(id)) match {
      case Some(ex) =>
        Ok(toJson(Map("status" -> "success", "code" -> ex.code)))
      case None =>
        Ok(toJson(Map("status" -> "error", "errormsg" -> "Unknown example")))
    }
  }

  def openConsole() = WebSocket.async[JsValue] { request =>
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

        (iteratee,enumerator)

      case InitFailure(error: String) =>
        // Connection error

        // A finished Iteratee sending EOF
        val iteratee = Done[JsValue,Unit]((),Input.EOF)

        // Send an error and close the socket
        val enumerator =  Enumerator[JsValue](toJson(
          Map("kind" -> "error", "message" -> error)
        )).andThen(Enumerator.enumInput(Input.EOF))

        (iteratee,enumerator)
    }
  }

}
