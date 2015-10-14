package leon.web
package controllers

import play.api._
import play.api.mvc._
import play.api.libs.iteratee._

import java.io.File

object Doc extends Controller {

  val dir = Play.current.configuration.getString("app.path").getOrElse("./")
  val prefix = "leon/target/sphinx/html/"

  def redirect = {
    Action {
      Redirect(routes.Doc.index)
    }
  }

  def index = {
    at("index.html")
  }

  def at(path: String) = Action {
    try {
       val f = new File(dir, prefix+path)
       Ok.sendFile(f, inline = true)//.withHeaders(CACHE_CONTROL -> "max-age=3600")
    } catch {
      case _: java.io.FileNotFoundException =>
        NotFound("File not found")
    }
  }
}
