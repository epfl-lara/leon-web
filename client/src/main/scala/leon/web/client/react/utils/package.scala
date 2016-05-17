/* Copyright 2009-2015 EPFL, Lausanne */

package leon.web
package client
package react

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._

package object utils {

  def octicon(name: String, content: String) = <.span(
    <.span(^.className := s"octicon octicon-$name"),
    content
  )

}

