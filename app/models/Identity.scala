/* Copyright 2009-2016 EPFL, Lausanne */

package leon.web
package models

import leon.web.shared.Provider

case class Identity(
  userId: User.UserId,
  provider: Provider,
  serviceUserId: Identity.ServiceUserId
) {
  def fullId = s"${provider.id}-${serviceUserId.value}"
}

object Identity {
  case class ServiceUserId(value: String) extends AnyVal
}

