/* Copyright 2009-2016 EPFL, Lausanne */

package leon.web
package client
package ops

import scala.scalajs.js
import js.Dynamic.{ literal => l }

import leon.web.shared.{RepositoryDesc, Project}

object tojs {

  implicit class RepositoryDescToJS(val repo: RepositoryDesc) extends AnyVal {
    def toJS = l(
      desc   = repo.desc,
      ofType = repo.ofType.id
    )
  }

  implicit class ProjectToJS(val project: Project) extends AnyVal {
    def toJS = l(
      repo   = project.repo.toJS,
      branch = project.branch,
      file   = project.file
    )
  }

}

