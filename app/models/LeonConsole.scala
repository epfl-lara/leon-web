package leon.web
package models

import akka.actor._
import akka.util.duration._
import akka.util.Timeout
import akka.dispatch.Await
import akka.dispatch.Future
import akka.pattern.ask

import play.api._
import play.api.libs.iteratee._
import play.api.libs.concurrent._
import play.api.libs.json._
import play.api.libs.json.Json.toJson


import play.api.Play.current

object LeonConsole {
  def open(remoteIP: String): Promise[(Iteratee[JsValue,_],Enumerator[JsValue])] = {
    import ConsoleProtocol._

    val session = Akka.system.actorOf(Props(new ConsoleSession(remoteIP)))
    implicit val timeout = Timeout(1.seconds)

    (session ? Init).asPromise.map {
      case InitSuccess(enumerator) =>
        // Create an Iteratee to consume the feed
        val iteratee = Iteratee.foreach[JsValue] { event =>
          session ! ProcessClientEvent(event)
        }.mapDone { _ =>
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
