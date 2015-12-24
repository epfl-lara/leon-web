/* Copyright 2009-2015 EPFL, Lausanne */

package leon.web
package client
package utils

import scala.scalajs.js

import upickle.Js
import upickle.default._

import leon.web.client.HandlersTypes._

object picklers {

  def Bool(x: Boolean): Js.Value =
    if (x) Js.True else Js.False

  implicit val hRepositoryWriter = Writer[HRepository] {
    case r =>
      Js.Obj(
        "name"          -> Js.Str(r.name),
        "fullName"      -> Js.Str(r.fullName),
        "owner"         -> Js.Str(r.owner),
        "defaultBranch" -> Js.Str(r.defaultBranch)
      )
  }

  implicit val hRepositoryReader = Reader[HRepository] {
    case Js.Obj(
        ("name",          Js.Str(_name)),
        ("fullName",      Js.Str(_fullName)),
        ("owner",         Js.Str(_owner)),
        ("defaultBranch", Js.Str(_defaultBranch))
      ) =>
        new HRepository {
          val id            = 0L
          val name          = _name
          val branches      = new js.Array[String]
          val cloneURL      = ""
          val defaultBranch = _defaultBranch
          val fork          = false
          val fullName      = _fullName
          val owner         = ""
          val size          = 0L
          val visibility    = "public"
        }
  }

  implicit val hBranchWriter = Writer[HBranch] {
    case b => Js.Obj(
      "name" -> Js.Str(b.name),
      "sha"  -> Js.Str(b.sha)
    )
  }

  implicit val hBranchReader = Reader[HBranch] {
    case Js.Obj(
        ("name", Js.Str(_name)),
        ("sha",  Js.Str(_sha))
      ) => new HBranch {
      val name = _name
      val sha  = _sha
    }
  }

}

