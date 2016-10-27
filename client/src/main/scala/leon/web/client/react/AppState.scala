/* Copyright 2009-2015 EPFL, Lausanne */

package leon.web
package client
package react

import scala.concurrent.Future
import monifu.reactive._
import monifu.reactive.subjects._

import leon.web.shared._
import leon.web.client.utils.Base64

import java.nio.ByteBuffer
import boopickle.Default._

case class AppState(

  // Currently logged-in user (if any)
  user              : Option[User] = None,

  // Repositories fetched from GitHub/Tequila
  repositories      : Option[Map[Provider, Seq[Repository]]] = None,

  // Currently selected repository
  repository        : Option[Repository] = None,

  // Available branches for selected repository
  branches          : Seq[Branch] = Seq(),

  // Currently selected branch
  branch            : Option[String] = None,

  // Scala files in the selected repository
  files             : Seq[String] = Seq(),

  // File+content loaded in the editor
  file              : Option[(String, String)] = None,

  // Whether or not to show the 'Load repository' modal
  showLoadRepoModal : Boolean = false,

  // Whether or not to show the 'Login' modal
  showLoginModal    : Boolean = false,

  // Whether or not to show the 'Account' modal
  showAccountModal  : Boolean = false,

  // Whether or not we are in the process of cloning `repository`
  isLoadingRepo     : Boolean = false,

  // Whether or not to treat the repo as a project
  treatAsProject    : Boolean = true,

  // Whether or not a user is logged-in
  isLoggedIn          : Boolean = false
) {

  lazy val repoState: Option[RepositoryState] = {
    for {
      r      <- repository
      b      <- branch
      (f, c) <- file
    }
    yield RepositoryState(
      repo      = r,
      branch    = b,
      file      = f,
      asProject = treatAsProject,
      code      = Some(c)
    )
  }

  lazy val unloadRepo: AppState =
    copy(
      repository     = None,
      branch         = None,
      branches       = Seq(),
      file           = None,
      files          = Seq(),
      treatAsProject = false
    )

  def serialize: String = {
    val buffer = Pickle.intoBytes(this)
    val array = new Array[Byte](buffer.remaining)
    buffer.get(array, 0, array.length)
    Base64.Encoder(array).toBase64
  }

}

object AppState {
  def unserialize(base64: String): Option[AppState] = {
    try {
      val array = Base64.Decoder(base64).toByteArray
      val buffer = ByteBuffer.wrap(array)
      Unpickle[AppState].tryFromBytes(buffer).toOption
    } catch {
      case err: Throwable =>
        println(err)
        None
    }
  }
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

