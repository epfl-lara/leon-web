/* Copyright 2009-2015 EPFL, Lausanne */

package leon.web
package client
package utils

import upickle._
import upickle.default
import shared.github._
import upickle.default._

object picklers {

  def Bool(x: Boolean): Js.Value =
    if (x) Js.True else Js.False

  implicit val hRepositoryWriter = Writer[Repository] {
    case r =>
      Js.Obj(
        "name"          -> Js.Str(r.name),
        "owner"         -> Js.Str(r.owner),
        "fullName"      -> Js.Str(r.fullName),
        "defaultBranch" -> Js.Str(r.defaultBranch)
      )
  }

  implicit val hRepositoryReader = Reader[Repository] {
    case Js.Obj(
        ("name",          Js.Str(_name)),
        ("owner",         Js.Str(_owner)),
        ("fullName",      Js.Str(_fullName)),
        ("defaultBranch", Js.Str(_defaultBranch))
      ) =>
        Repository(
          id            = RepositoryId(0L),
          name          = _name,
          owner         = _owner,
          fullName      = _fullName,
          branches      = Array[Branch](),
          cloneURL      = "",
          defaultBranch = _defaultBranch,
          fork          = false,
          size          = 0L,
          visibility    = Public
        )
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

