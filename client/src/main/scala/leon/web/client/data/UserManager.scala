package leon.web
package client
package data

import scalajs.js
import js.Dynamic.{global => g}
import leon.web.shared._

object UserManager {
  
  def parseIdentities(ids: js.Dynamic): Set[Identity] = {
    val dict = ids.asInstanceOf[js.Dictionary[js.Dynamic]]
    (for((k, i) <- dict) yield {
      parseIdentity(i)
    }).toSet
  }
  
  def parseIdentity(id: js.Dynamic): Identity = {
    val dict = id.asInstanceOf[js.Dictionary[String]]
    
    Identity(
      serviceUserId= ServiceUserId(dict("serviceUserId")),
      provider=  Provider(dict("provider")),
      firstName= dict.get("firstName"),
      lastName=  dict.get("lastName"),
      fullName=  dict.get("fullName"),
      email=     dict.get("email").map(Email),
      avatarUrl= dict.get("avatarUrl")
    )
  }
  
  private
  lazy val _initial: Option[User] = {
    val userJs = g._leon_user.asInstanceOf[js.Dictionary[js.Dynamic]]
    
    val user = User(
      userId = UserId(userJs("id").asInstanceOf[String]),
      main   = userJs.get("main").map(parseIdentity),
      identities = userJs.get("identities").map(parseIdentities).getOrElse(Set())
    )
    
    Option(user)
  }

  lazy val initial: Option[User] = _initial
}

