package leon.web
package models

import anorm._
import play.api.libs.json._
import play.api.db._
import play.api.Play.current
import java.security.MessageDigest

object Permalink {
  def hash(str: String, offset: Int): String = {
    val bytes : Array[Byte] = str.getBytes("UTF-8")
    val digest = MessageDigest.getInstance("MD5")

    val sb = new StringBuilder()
    for(b <- digest.digest(bytes)) {
      sb.append(Integer.toHexString((b & 0xFF) | 0x100).substring(1,3))
    }

    sb.toString+"-"+offset
  }
  def store(code: String): Option[String] = {
    implicit val c = DB.getConnection()

    var offset = 0

    var link = ""
    var rehash = false;
    var store = true;

    do {
      offset += 1

      link = hash(code, offset)

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
        .on("link" -> link, "code" -> code).execute
    }

    Some(link)
  }

  def get(link: String): Option[String] = {
    implicit val c = DB.getConnection()

    val res = SQL("SELECT code FROM permalinks WHERE link = {link}").on("link" -> link).apply()

    res.headOption.map(_[String]("code"))
  }

}

