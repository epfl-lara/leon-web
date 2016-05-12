/* Copyright 2009-2015 EPFL, Lausanne */

package leon.web
package client
package react
package components
package git

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._

/** git diff view */
object DiffView {

  case class Props(diff: String)

  class Backend($: BackendScope[Props, Unit]) {

    def render(props: Props) = {
      <.div(^.className := "git-diff-view",
        <.pre(^.className := "diff",
          colorize(props.diff)
        )
      )
    }

    def isHeader(line: String): Boolean =
      if (line.length >= 3) {
        val first3 = line.substring(0, 3)
        first3 === "---" || first3 === "+++"
      }
      else false

    def colorize(diff: String) =
      diff.linesWithSeparators.zipWithIndex.map { case (line, n) =>
        line.headOption match {
          case _ if isHeader(line) => <.span(^.className := "diff-header",   ^.key := s"line-$n", line)
          case Some('+')           => <.span(^.className := "diff-addition", ^.key := s"line-$n", line)
          case Some('-')           => <.span(^.className := "diff-deletion", ^.key := s"line-$n", line)
          case Some('@')           => <.span(^.className := "diff-chunk",    ^.key := s"line-$n", line)
          case _                   => <.span(line, ^.key := s"line-$n")
      } }
  }

  val component =
    ReactComponentB[Props]("Git.DiffView")
      .stateless
      .renderBackend[Backend]
      .build

  def apply(diff: String) =
    component(Props(diff))
}

