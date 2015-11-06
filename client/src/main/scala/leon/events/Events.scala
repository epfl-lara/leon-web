package leon.web.client
package events

import monifu.reactive._
import monifu.reactive.subjects._

import leon.web.client.HandlersTypes._

sealed trait Event
case class RepositoriesLoaded(repos: Seq[HRepository]) extends Event
case class FilesLoaded(files: Seq[String]) extends Event
case class FileLoaded(fileName: String, content: String) extends Event
case class CodeUpdated() extends Event

object Events {

  val repositoriesLoaded = PublishSubject[RepositoriesLoaded]()
  val filesLoaded        = PublishSubject[FilesLoaded]()
  val fileLoaded         = PublishSubject[FileLoaded]()
  val codeUpdated        = PublishSubject[CodeUpdated]()

}

