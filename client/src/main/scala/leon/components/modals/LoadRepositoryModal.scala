package leon.web.client
package components
package modals

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._

import monifu.concurrent.Implicits.globalScheduler
import monifu.concurrent.Cancelable
import monifu.reactive.Observable
import monifu.reactive.subjects.PublishSubject

import leon.web.client.HandlersTypes.HRepository

import leon.web.client.react.attrs._
import leon.web.client.syntax.subject._

object LoadRepositoryModal {

  sealed trait Event
  case class RepositoriesLoaded(repos: Seq[HRepository]) extends Event
  case class LoadRepositories() extends Event
  case class LoadRepository(repo: HRepository) extends Event

  type Channel = PublishSubject[Event]
  def channel(): Channel = PublishSubject[Event]()

  case class Props(chan: Channel, modalChan: Modal.Channel)
  case class State(repos: Option[Seq[HRepository]] = None, selectedRepo: Option[HRepository] = None)

  class Backend($: BackendScope[Props, State]) {
    var subscription: Option[Cancelable] = None

    def onMount() = $.props map { props =>
      val cancel = props.chan.doWork(onEvent).subscribe()
      subscription = Some(cancel)
    }

    def onUnmount() = Callback {
      subscription.foreach(_.cancel())
    }

    def onEvent(cmd: Event): Unit = cmd match {
      case RepositoriesLoaded(repos) =>
        $.modState(_.copy(repos = Some(repos))).runNow()

      case _ =>
    }

    def onShow(): Unit =
      $.props.map(_.chan ! LoadRepositories()).runNow()

    def onHide(): Unit = {}

    def onSelectRepo(repo: HRepository) = {
      $.modState(_.copy(selectedRepo = Some(repo))).runNow()
    }

    def onLoadRepo(): Callback =
      $.props.zip($.state) map { case (props, state) =>
        state.selectedRepo foreach { repo =>
          println("Will load ", repo)
          props.chan ! LoadRepository(repo)
        }
      }

    val cancelButton =
      <.button(
        ^.`class`   := "btn",
        dataDismiss := "modal",
        "Cancel"
      )

    val loadButton =
      <.a(
        ^.`class` := "btn btn-primary",
        ^.role    := "button",
        ^.onClick --> onLoadRepo,
        "Load"
      )

    val loading =
      <.p("Loading...")

    def render(props: Props, state: State) =
      Modal(props.modalChan, onShow, onHide)(
        <.div(^.`class` := "modal-header",
          Modal.closeButton,
          <.h3("Load a repository from GitHub")
        ),
        <.div(^.`class` := "modal-body",
          <.p(
            """Pick a repository to load from the list below:"""
          ),
          state.repos match {
            case None        => loading
            case Some(repos) => RepositoryList(repos, onSelect = onSelectRepo)
          }
        ),
        <.div(^.`class` := "modal-footer",
          if (state.selectedRepo.isDefined) loadButton else EmptyTag,
          cancelButton
        )
      )
  }

  val component =
    ReactComponentB[Props]("LoadRepositoryModal")
      .initialState(State())
      .renderBackend[Backend]
      .componentDidMount(_.backend.onMount())
      .componentWillUnmount(_.backend.onUnmount())
      .build

  def apply(chan: Channel)(modalChan: Modal.Channel) =
    component(Props(chan, modalChan))

}

