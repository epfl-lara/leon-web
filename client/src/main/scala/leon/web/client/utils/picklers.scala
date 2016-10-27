/* Copyright 2009-2015 EPFL, Lausanne */

package leon.web
package client
package utils

import upickle._
import upickle.default._

import leon.web.shared.{User, Identity}
import leon.web.shared._

object picklers {

  def Bool(x: Boolean): Js.Value =
    if (x) Js.True else Js.False

  implicit val UserWriter = Writer[User] {
    case u =>
      Js.Obj("id" -> Js.Str(u.userId.value))
  }

  implicit val UserReader = Reader[User] {
    case Js.Obj(("id", Js.Str(_id))) =>
      User(UserId(_id), None, Set.empty[Identity])
  }

  implicit val providerWriter = Writer[Provider] {
    case p => Js.Str(p.id)
  }

  implicit val providerReader = Reader[Provider] {
    case Js.Str(_id) => Provider(_id)
  }

  implicit val hBranchWriter = Writer[Branch] {
    case b => Js.Obj(
      "name" -> Js.Str(b.name),
      "sha"  -> Js.Str(b.sha)
    )
  }

  implicit val hBranchReader = Reader[Branch] {
    case Js.Obj(
        ("name", Js.Str(_name)),
        ("sha",  Js.Str(_sha))
      ) => Branch(
      name = _name,
      sha  = _sha
    )
    }

}

