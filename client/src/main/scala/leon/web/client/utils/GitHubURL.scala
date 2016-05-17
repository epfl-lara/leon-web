/* Copyright 2009-2015 EPFL, Lausanne */

package leon.web
package client
package utils

import scala.scalajs.js
import scala.scalajs.js.annotation._

@js.native
trait GitHubURL extends js.Object {
  val user: String
  val repo: String
  val branch: String
  val repopath: String
}

object GitHubURL {
  def parse(url: String): Option[GitHubURL] = {
    val ghUrl = parseGitHubURL(url)
    if (isDefined(ghUrl)) Some(ghUrl) else None
  }

  def isDefined(url: GitHubURL): Boolean =
    url != null &&
    url.user != null && url.repo != null &&
    url.branch != null && url.repopath != null
}

@JSName("leonParseGitHubURL")
@js.native
object parseGitHubURL extends js.Object {

  def apply(url: String): GitHubURL = js.native

}

