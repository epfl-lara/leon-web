/* Copyright 2009-2015 EPFL, Lausanne */

package leon.web
package client
package react

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

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

  // Whether or not to show the 'Account' modal
  showAccountModal  : Boolean                  = false,

  // Whether or not we are in the process of cloning `repository`
  isLoadingRepo     : Boolean                  = false,

  // Whether or not to treat the repo as a project
  treatAsProject    : Boolean                  = true,

  // Whether or not a user is logged-in
  isLoggedIn          : Boolean                = false
) {

  lazy val currentProject: Option[Project] = {
    for {
      r      <- repository
      b      <- branch
      (f, c) <- file
    }
    yield Project(
      owner  = r.owner,
      repo   = r.name,
      branch = b,
      file   = f,
      code   = Some(c)
    )
  }

  lazy val unloadProject: AppState =
    copy(
      repository     = None,
      branch         = None,
      branches       = Seq(),
      file           = None,
      files          = Seq(),
      treatAsProject = false
    )

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
   val updates: BehaviorSubject[AppState => Future[AppState]] =
    BehaviorSubject(Future.successful)

  /** Listen for new transformations and applies them to the current state. */
  val asObservable: Observable[AppState] =
    updates.flatScan(initial) { (state, op) =>
      Observable.fromFuture {
        op(state)
      }
    }.distinctUntilChanged
}

object GlobalAppState {
  def apply()                  = new GlobalAppState(AppState())
  def apply(initial: AppState) = new GlobalAppState(initial)
}

