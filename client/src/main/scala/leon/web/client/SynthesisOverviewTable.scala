package leon.web
package client

import shared.messages.{Event => _, _}
import scala.scalajs.js

object SynthesisOverviewTable {
  import japgolly.scalajs.react._
  import japgolly.scalajs.react.vdom.prefix_<^._
  case class Props(getOverviewFunctions: () => js.Dictionary[OverviewFunction], onSynthesisChosenRule: Option[String] => Unit)
  //case class State(data: SynthesisOverview)
  case class State(isCompiled: Boolean, data: SynthesisOverview, rulesData: Map[(String, Int), HSynthesisRulesToApply])
  
  class Backend($: BackendScope[Props, State]) {
    def changeState(isCompiled: Boolean, data: SynthesisOverview, rulesData: Map[(String, Int), HSynthesisRulesToApply]) = {
      $.setState(State(isCompiled, data, rulesData)).runNow()
    }
    
    def render(props: Props, state: State) = {
      def menu(index: Int, fname: String, description: String, cid: Int) = {
        val id = "menu" + fname + index
        
        val ruleApps = state.rulesData.get((fname, cid)) match {
          case Some(HSynthesisRulesToApply(_, _, ruleApps)) if state.isCompiled =>
            for (app <- ruleApps) yield {
              val closed = app.status == "closed"
              val rid = app.id
              <.li(
                ^.role := "presentation",
                ^.classSet1("temp",
                    "disabled" -> closed
                ),
                <.a(
                  ^.role := "menuitem", ^.tabIndex:="-1", ^.href :="#",
                  ^.action :="rule", "cid".reactAttr := cid, "rid".reactAttr := rid,
                  closed ?= <.i(^.className := "a fa-exclamation-circle"),
                  app.name,
                  ^.onClick --> Callback{ 
                    props onSynthesisChosenRule Some(rid.toString)
                    Backend.synthesis.doApplyRule(fname, cid, rid)
                  }
                )
              )
            }
          case _ => Array(
            <.li(^.role:="presentation", ^.className:="disabled loader temp",
              <.a(^.role:="menuitem", ^.tabIndex:="-1", 
                <.img(^.src:="/assets/images/loader.gif")
              )
            ))
        }
  
        <.div(^.className :="dropdown",
          <.a(^.id:= id, ^.href:="#", ^.role:="button", ^.className:="dropdown-toggle", "data-toggle".reactAttr:="dropdown",
            <.i(^.className:="fa fa-magic"), " ",
            description,
            ^.onClick --> (Callback{
              if(state.isCompiled) {
                Backend.synthesis.getRulesToApply(fname, cid)
              }
            })
          ),
          <.ul(^.className := "dropdown-menu", ^.role:="menu", "aria-labelledby".reactAttr := id,
            if (state.isCompiled) {
              List(
                <.li(^.role:="presentation",
                    <.a(^.role:="menuitem", ^.tabIndex:="-1", ^.href:="#", ^.action:="search", "cid".reactAttr := index,
                        "Search",
                        ^.onClick --> Callback{
                          props onSynthesisChosenRule Some("search")
                          Backend.synthesis.search(fname, cid)
                        })),
                <.li(^.role:="presentation",
                    <.a(^.role:="menuitem", ^.tabIndex:="-1", ^.href:="#", ^.action:="explore", "cid".reactAttr := index,
                        "Explore",
                        ^.onClick --> Callback{
                          props onSynthesisChosenRule None
                          Backend.synthesis.explore(fname, cid)
                        }
                    )),
                <.li(^.role:="presentation", ^.className:="divider")
              ) ++ ruleApps
            } else {
              List(
                <.li(^.role:="presentation", ^.className:="disabled loader temp",
                  <.a(^.role:="menuitem", ^.tabIndex:="-1", ^.href := "#", "fname".reactAttr := fname,
                    <.i(^.className:="fa fa-ban"), "Not yet compiled..."
              )))
            }
          )
        )
      }
  
      val fnamesOpt: Option[Array[String]] = state.data.functions.map(ff => ff.unzip._1.toArray)
      val fnames = fnamesOpt.getOrElse[Array[String]](Array[String]()).sorted
      val functions = props.getOverviewFunctions()
  
      <.table(
          ^.id:="synthesis_table", ^.className :="table table-hover table-condensed",
          <.tbody(
        (for (f <- fnames) yield {
          functions.get(f) match {
            case Some(function) =>
              if (state.data.functions.get(f).length == 1) {
                val sp = state.data.functions.get(f)(0)
                List(<.tr(
                  <.td(
                    ^.className := "fname problem  clicktoline",
                    "line".reactAttr := sp.line,
                    "fname".reactAttr := f,
                    "cid".reactAttr := sp.index,
                    menu(sp.index, f, function.displayName, sp.index)
                  )
                ))
              } else {
                val head = <.tr(
                  <.td(
                    ^.className := "fname  clicktoline",
                    "line".reactAttr := function.line,
                    function.displayName
                  )
                )
                val spArray = state.data.functions.get(f)
                head :: (for (i <- 0 until spArray.length) yield {
                  val sp = spArray(i)
                  <.tr(
                    <.td(
                      ^.className:="problem subproblem clicktoline",
                      "line".reactAttr := sp.line,
                      "fname".reactAttr := f,
                      "cid".reactAttr := sp.index,
                      menu(sp.index, f, sp.description, sp.index)
                    )
                  )
                }).toList
              }
            case None => Nil
          }
        }).toList
      ))
    }
  }
  val component = ReactComponentB[Props]("SynthesisTable")
    .initialState(State(false, SynthesisOverview(None), Map()))
    .renderBackend[Backend]
    .build
  def apply(getOverviewFunctions: () => js.Dictionary[OverviewFunction],
            onSynthesisChosenRule: Option[String] => Unit) =
    component(Props(getOverviewFunctions, onSynthesisChosenRule))
}