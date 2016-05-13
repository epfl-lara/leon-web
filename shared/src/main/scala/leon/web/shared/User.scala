package leon.web
package shared

case class UserId(value: String)// extends AnyVal

case class User (
  userId:     UserId,
  main:       Option[Identity],
  identities: Set[Identity]
) {

  lazy val github  = identity(GitHubProvider)
  lazy val tequila = identity(TequilaProvider)
  
  def identity(provider: Provider): Option[Identity] =
    identities.find(_.provider == provider)

  def unlink(id: Identity): User = {
    require (identities.size >= 2)

    val newIds  = identities - id
    val newMain = if (main.map(i => id == i).getOrElse(false)) Some(newIds.head) else main

    User(userId, newMain, newIds)
  }
}

object User {
  def apply(userId: UserId, main: Provider, ids: Set[Identity]): User =
    User(userId, ids.find(_.provider === main), ids)
}