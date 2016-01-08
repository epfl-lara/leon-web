/* Copyright 2009-2015 EPFL, Lausanne */

package leon.web
package client
package react

import monifu.reactive._
import monifu.reactive.subjects._

import leon.web.client.HandlersTypes._
import leon.web.shared.Project

import upickle.default._
import leon.web.client.utils.picklers._

case class AppState(
  // Repositories fetched from GitHub API
  repositories      : Option[Seq[HRepository]] = None,

  // Currently selected repository
  repository        : Option[HRepository]      = None,

  // Available branches for selected repository
  branches          : Seq[HBranch]             = Seq(),

  // Currently selected branch
  branch            : Option[String]           = None,

  // Scala files in the selected repository
  files             : Seq[String]              = Seq(),

  // File+content loaded in the editor
  file              : Option[(String, String)] = None,

  // Whether or not to show the 'Load repository' modal
  showLoadRepoModal : Boolean                  = false,

  // Whether or not to show the 'Login' modal
  showLoginModal    : Boolean                  = false,

  // Whether or not we are in the process of cloning `repository`
  isLoadingRepo     : Boolean                  = false,

  // The current project, if any
  currentProject    : Option[Project]          = None,

  // Whether or not to treat the repo as a project
  treatAsProject    : Boolean                  = true,

  // Whether or not a user is logged-in
  isLoggedIn          : Boolean                = false
) {

  def toJSON: String = {
    write(this)
  }

}

object AppState {
  def fromJSON(json: String): AppState =
    read[AppState](json)
}

/** This objects holds the whole React application state,
  * and exposes a way to apply transformations to the state,
  * as well as an [[monifu.reactive.Observable]] that can be used
  * to track its modifications.
  */
class GlobalAppState(val initial: AppState) {

  /** Tracks state transformations, the result of which can be observed
    * by subsribing to `asObservable`. */
  val updates = BehaviorSubject[AppState => AppState]((x: AppState) => x)

  /** Listen for new transformations and applies them to the current state. */
  val asObservable: Observable[AppState] = updates.scan(initial) { (state, op) =>
    op(state)
  }.distinctUntilChanged
}

object GlobalAppState {
  def apply()                  = new GlobalAppState(AppState())
  def apply(initial: AppState) = new GlobalAppState(initial)
}

