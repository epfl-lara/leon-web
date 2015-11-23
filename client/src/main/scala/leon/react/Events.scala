/* Copyright 2009-2015 EPFL, Lausanne */

package leon.web.client
package react

import monifu.reactive._
import monifu.reactive.subjects._

import leon.web.client.HandlersTypes._

/** Events triggered in reaction to the [[leon.web.client.react.Action]]s.
  * These events can be listened to, and are meant to trigger state
  * transformations can will themselves trigger a re-render of the app.
  */
sealed trait Event
case class RepositoriesLoaded(repos: Seq[HRepository]) extends Event
case class RepositoryLoaded(files: Seq[String], branches: Seq[HBranch]) extends Event
case class FileLoaded(fileName: String, content: String) extends Event
case class BranchChanged(branch: String, files: Seq[String]) extends Event
case class CodeUpdated() extends Event
case class GitProgress(task: String, percentage: Option[String]) extends Event

/** Exposes those events as [[monifu.reactive.subjects.Subject]]. */
object Events {

  val repositoriesLoaded = PublishSubject[RepositoriesLoaded]()
  val repositoryLoaded   = PublishSubject[RepositoryLoaded]()
  val fileLoaded         = PublishSubject[FileLoaded]()
  val branchChanged      = PublishSubject[BranchChanged]()
  val codeUpdated        = PublishSubject[CodeUpdated]()
  val gitProgress        = PublishSubject[GitProgress]()

}

