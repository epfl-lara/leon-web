/* Copyright 2009-2015 EPFL, Lausanne */

package leon.web
package client
package react

import monifu.reactive.subjects._

import leon.web.client.HandlersTypes._
import leon.web.client.data.User

/** Events triggered in reaction to the [[leon.web.client.react.Action]]s.
  * These events can be listened to, and are meant to trigger state
  * transformations can will themselves trigger a re-render of the app.
  */
sealed trait Event

final case class RepositoryLoaded(
  repo: HRepository,
  files: Seq[String],
  branches: Seq[HBranch],
  currentBranch: String
) extends Event

final case class RepositoriesLoaded(repos: Seq[HRepository])           extends Event
final case class FileLoaded(fileName: String, content: String)         extends Event
final case class BranchChanged(branch: String, files: Seq[String])     extends Event
final case class CodeUpdated(code: String)                             extends Event
final case class GitProgress(task: String, percentage: Option[String]) extends Event
final case class GitOperationDone(result: HGitOperationResult)         extends Event
final case class UserUpdated(user: User.Raw)                           extends Event

/** Exposes those events as [[monifu.reactive.subjects.Subject]]. */
object Events {

  final val repositoriesLoaded = PublishSubject[RepositoriesLoaded]() // dump "RepositoriesLoaded"
  final val repositoryLoaded   = PublishSubject[RepositoryLoaded  ]() // dump "RepositoryLoaded"
  final val fileLoaded         = PublishSubject[FileLoaded        ]() // dump "FileLoaded"
  final val branchChanged      = PublishSubject[BranchChanged     ]() // dump "BranchChanged"
  final val codeUpdated        = PublishSubject[CodeUpdated       ]() // dump "CodeUpdated"
  final val gitProgress        = PublishSubject[GitProgress       ]() // dump "GitProgress"
  final val gitOperationDone   = PublishSubject[GitOperationDone  ]() // dump "GitOperationDone"
  final val userUpdated        = PublishSubject[UserUpdated       ]()  dump "UserUpdated"

}

