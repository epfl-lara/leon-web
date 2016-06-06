package leon.web
package client

import org.scalajs.jquery.{ jQuery => $ }

import JQueryExtended.toJQueryExtended

object WebMode {
  def activate() = {
    $(".panel > h3, #title, #overview .progress-bar, #invariant .progress-bar").addClass("webdesign", 800)
    $("#htmlDisplayerDiv").show()
  }
  def deactivate() = {
    $(".panel > h3, #title, #overview .progress-bar, #invariant .progress-bar").removeClass("webdesign", 800)
    $("#htmlDisplayerDiv").hide()
  }
}