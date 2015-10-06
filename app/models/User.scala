
package leon.web
package models

import securesocial.core._

case class User(main: BasicProfile, identities: List[BasicProfile])

