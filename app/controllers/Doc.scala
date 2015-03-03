package leon.web
package controllers

import play.api._
import play.api.mvc._
import play.api.libs.iteratee._
import play.api.libs.concurrent._
import play.api.libs.json._
import play.api.libs.json.Json._
import play.api.libs.json.Writes._

import java.io.File

object Doc extends Controller {
  val prefix = "leon/doc/_build/html/"

  def index = {
    at("index.html")
  }

  def at(path: String) = {
    _root_.controllers.ExternalAssets.at("leon/doc/_build/html/", path)
  }
}
