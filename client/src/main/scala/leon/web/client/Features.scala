package leon.web
package client

import scala.scalajs.js
import scala.scalajs.js.annotation._
import scala.scalajs.js.JSON
import org.scalajs.dom.ext.LocalStorage

@ScalaJSDefined
class Feature(_a: Boolean, _n: String, _m: Either[shared.Module, String]) extends js.Object {
  var active: Boolean = _a
  val name: String = _m.fold((m: shared.Module) => m.name, (i : String) => i )
  val displayName: String = _n
  val module: Option[shared.Module] = _m.fold(a => Some(a), _ => None)
  
  def toggle(): Boolean = {
    active = !active
    Main.regenerateFeaturesPanel()
    module match {
      case Some(module) =>
        Backend.main.setFeatureActive(module, active)
      case None =>
    }
    LocalStorage.update("leonFeatures", JSON.stringify(Main.Features.toJsObject));
    active
  }
  
  def activate(): Unit = {
    if(!active) toggle()
  }
  
  def deactivate(): Unit = {
    if(active) toggle()
  }
}

object FeaturesMappings {
  var stringToModule = Map[String, shared.Module]()
  var moduleToFeature = Map[shared.Module, Feature]()
  var stringToFeature = Map[String, Feature]()
}
