/* Copyright 2009-2015 EPFL, Lausanne */

package leon.web
package client
package react
package components

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._

/** Simple dropdown menu, currently implemented as a `<select />`. */
object Dropdown {

  case class Props[A](
    items: Seq[A],
    header: Option[String],
    empty: Option[String],
    renderOption: A => String,
    onSelect: A => Callback,
    selected: Option[A] = None,
    className: String = ""
  )

  class Backend[A]($: BackendScope[Props[A], Unit]) {

    def onSelect(map: Map[String, A])(e: ReactEventI): Callback =
      e.preventDefaultCB >>
      $.props.flatMap(_.onSelect(map(e.target.value)))

    def header(label: String) =
      <.option(^.value := "", ^.disabled := true, label)

    def toOptions(emptyLabel: Option[String], items: Seq[String]) =
      if (items.isEmpty) {
        emptyLabel match {
          case None        => Seq()
          case Some(label) => Seq(header(label))
        }
      }
      else {
        items map { item =>
          <.option(^.value := item, ^.key := item, item)
        }
      }

    val classNames = Seq("form-control", "panel-element-full")

    def render(props: Props[A]) = {
      val selectedItem      = props.selected map props.renderOption
      val renderedItems     = props.items map props.renderOption
      val itemsMap          = Map((renderedItems zip props.items): _*)
      val asOptions         = toOptions(props.empty, renderedItems)

      val options = props.header match {
        case None        => asOptions
        case Some(label) => header(label) +: asOptions
      }

      <.select(
        ^.className := (props.className +: classNames) mkString " ",
        ^.onChange ==> onSelect(itemsMap),
        ^.value := selectedItem.filter(itemsMap contains _).getOrElse(""),
        options
      )
    }
  }

  def apply[A](items: Seq[A], header: Option[String], empty: Option[String],
               renderOption: A => String, onSelect: A => Callback,
               selected: Option[A] = None, className: String = "") = {

    val component =
      ReactComponentB[Props[A]]("Dropdown")
        .renderBackend[Backend[A]]
        .build

    component(Props(
      items        = items,
      header       = header,
      empty        = empty,
      renderOption = renderOption,
      onSelect     = onSelect,
      selected     = selected,
      className    = className
    ))
  }

}

