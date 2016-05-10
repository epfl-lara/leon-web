/* Copyright 2009-2015 EPFL, Lausanne */

package leon.web
package client
package react

import leon.web.shared.messages._

/** Register WebSocket handlers, and push the received messages
  * through the appropriate event bus.
  *
  * @see [[leon.web.client.events.Event]]
  */
object Handlers extends (Message => Boolean) {
  def apply(m: Message): Boolean = m match {
    case data: RepositoriesLoaded => Events.repositoriesLoaded onNext data ; true
    case data: RepositoryLoaded => Events.repositoryLoaded onNext data ; true
    case data: FileLoaded => Events.fileLoaded onNext data ; true
    case data: BranchChanged =>
      if (data.success) {
        Events.branchChanged onNext data
      }
      true
    case data: GitProgress => Events.gitProgress onNext data ; true
    case data: GitOperationDone => Events.gitOperationDone onNext data ; true
    case _ => false
  }
}

