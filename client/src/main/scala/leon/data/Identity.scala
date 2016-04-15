
package leon.web
package client
package data

import scala.scalajs.js
import js.Dynamic.global
import js.annotation.ScalaJSDefined

import leon.web.shared.Provider

case class Identity(
  userId:    String,
  provider:  Provider,
  email:     String,
  firstName: Option[String],
  lastName:  Option[String],
  fullName:  Option[String],
  avatarUrl: Option[String]
) {

  def name: Option[String] = {
    lazy val firstLast =
      firstName
        .zip(lastName)
        .headOption
        .map { case (f, l) => s"$f $l" }

    fullName orElse firstLast
  }
}

object Identity {

  def apply(i: Identity.Raw): Identity =
    Identity(
      i.userId, Provider(i.provider), i.email,
      Option(i.firstName), Option(i.lastName),
      Option(i.fullName), Option(i.avatarUrl)
    )

  @ScalaJSDefined
  trait Raw extends js.Object {
    val userId:    String
    val provider:  String
    val email:     String
    val firstName: String
    val lastName:  String
    val fullName:  String
    val avatarUrl: String
  }
}

