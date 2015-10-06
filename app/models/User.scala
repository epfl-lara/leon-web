
package leon.web
package models

import anorm._
import play.api.db._
import play.api.Play.current

import securesocial.core._

case class User(profile: BasicProfile)

object User {

  def setup(): Unit = {
    implicit val conn = DB.getConnection()

    SQL"""
      CREATE TABLE IF NOT EXISTS users (
        provider_id VARCHAR(40),
        user_id VARCHAR(40),
        first_name VARCHAR,
        lastName VARCHAR,
        full_name VARCHAR,
        email VARCHAR,
        avatar_url VARCHAR,
        access_token VARCHAR,

        PRIMARY KEY (provider_id, user_id)
      )
    """.execute
  }

}

