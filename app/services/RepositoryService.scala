/* Copyright 2009-2015 EPFL, Lausanne */

package leon.web
package services

import scala.collection.JavaConverters._

import java.io.File
import play.Play

import leon.web.models.{User, RepositoryInfos}
import leon.web.shared.{GitOperation, Project}

object RepositoryService {

  lazy val root = {
    Play.application.configuration.getString("repositories.path")
  }

  def repositoryFor(user: User, owner: String, name: String, token: Option[String] = None): RepositoryInfos = {
    val path = new File(s"$root/${user.fullId}/$owner/$name")
    new RepositoryInfos(path, user, token)
  }

  def perform(op: GitOperation, wc: RepositoryInfos, project: Project): (Boolean, Option[Map[String, Set[String]]]) = {
    op match {
      case GitOperation.Status =>
        val status = wc.status()
        val data = Map(
          "added"     -> status.getAdded(),
          "changed"   -> status.getChanged(),
          "modified"  -> status.getModified(),
          "removed"   -> status.getRemoved(),
          "untracked" -> status.getUntracked()
        ).mapValues(_.asScala.toSet)

        (true, Some(data))

      case GitOperation.Push =>
        (wc.push(), None)

      case GitOperation.Pull =>
        (wc.pull(), None)

      case GitOperation.Reset =>
        (wc.reset(hard = true), None)

      case GitOperation.Commit(message) =>
        val success = wc.add(project.file) && wc.commit(message)
        (success, None)

    }

  }

}

