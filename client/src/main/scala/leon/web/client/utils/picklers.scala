/* Copyright 2009-2015 EPFL, Lausanne */

package leon.web
package client
package utils

import scala.scalajs.js

//import boopickle.Js
import boopickle.Default._

import leon.web.shared.HandlerMessages._
/*
object picklers {
  def Bool(x: Boolean): Js.Value =
    if (x) Js.True else Js.False

  implicit val hRepositoryWriter = Writer[HRepository] {
    case r =>
      Js.Obj(
        "name"          -> Js.Str(r.name),
        "owner"         -> Js.Str(r.owner),
        "fullName"      -> Js.Str(r.fullName),
        "defaultBranch" -> Js.Str(r.defaultBranch)
      )
  }

  implicit val hRepositoryReader = Reader[HRepository] {
    case Js.Obj(
        ("name",          Js.Str(_name)),
        ("owner",         Js.Str(_owner)),
        ("fullName",      Js.Str(_fullName)),
        ("defaultBranch", Js.Str(_defaultBranch))
      ) =>
        new HRepository {
          val id            = 0L
          val name          = _name
          val owner         = _owner
          val fullName      = _fullName
          val branches      = new js.Array[String]
          val cloneURL      = ""
          val defaultBranch = _defaultBranch
          val fork          = false
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

}*/

