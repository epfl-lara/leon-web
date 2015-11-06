package leon.web.client
package react

import monifu.reactive._
import monifu.reactive.subjects._

import leon.web.client.HandlersTypes._
import leon.web.client.events.GitProgress

case class AppState(
  repositories      : Option[Seq[HRepository]] = None,
  repository        : Option[HRepository]      = None,
  files             : Seq[String]              = Seq(),
  file              : Option[(String, String)] = None,
  showLoadRepoModal : Boolean                  = false,
  isLoadingRepo     : Boolean                  = false
)

class GlobalAppState {
  val initial      = AppState()
  val updates      = BehaviorSubject((x: AppState) => x)
  val asObservable = updates.scan(initial) { (state, op) =>
    op(state)
  }
}

object GlobalAppState {
  def apply() = new GlobalAppState()
}

