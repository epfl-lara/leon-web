package leon.web
package repositories

import anorm._
import play.api.libs.json._
import play.api.db._
import play.api.Play.current

import leon.web.utils.Hash
import leon.web.models.{Permalink, Code, Link}

object PermalinkRepository {

  def store(code: Code): Option[Permalink] = {
    implicit val c = DB.getConnection()

    var offset = 0

    var link = Link("")
    var rehash = false
    var store = true

    do {
      offset += 1

      link = Link(Hash.hash(code.value, offset))

      val res = get(link)
      if (res.isDefined) {
        if (res != Some(code)) {
          rehash = true
        } else {
          store = false
        }
      }
    } while(rehash)

    if (store) {
      SQL("INSERT INTO permalinks (link, code) VALUES ({link}, {code})")
        .on("link" -> link.value, "code" -> code.value).execute
    }

    Some(Permalink(link, code))
  }

  def get(link: Link): Option[Permalink] = {
    implicit val c = DB.getConnection()

    val res = SQL("SELECT link, code FROM permalinks WHERE link = {link}").on("link" -> link.value).apply()

    res.headOption.map { row =>
      val link = Link(row[String]("link"))
      val code = Code(row[String]("code"))
      Permalink(link, code)
    }
  }

}

