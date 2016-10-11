/* Copyright 2009-2015 EPFL, Lausanne */

package leon.web
package stores

import anorm._
import play.api.db._
import play.api.Play.current

import leon.web.utils.Hash
import leon.web.models.{Permalink, Code, Link}

object PermalinkStore {

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
        val permalink = Permalink(link, code)
        if (res =!= Some(permalink)) {
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

