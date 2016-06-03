import leon.collection._
import leon.webDSL.webDescription._
import leon.webDSL.webBuilding._
import implicits._
import scala.language.dynamics

object Reflex4YouTranslations {
  val translations = Map(
  "eng" -> Map(
    "websiteurl" -> "reflex4you.com",
    "welcome_to" -> "Welcome to ",
    "a_cool_website" -> ", a website where you can create customized mathematical art from friends’ names.",
    "Please" -> "Please ",
    "writeyourname" -> "write your name",
    "ibelow" -> " below",
    "ihere" -> " here")
  )
  case class Translations(language: String) extends Dynamic {
    def mapping = translations.getOrElse(language, translations("eng"))
    def selectDynamic(arg: String) = mapping.getOrElse(arg, "")
  }
  
  def main = render("en")
  def render(language: String) = {
    val t = Translations(language)
    WebPage(
        <.div(
            <.h1(t.welcome_to + t.websiteurl),
            <.p(t.welcome_to + t.websiteurl + t.a_cool_website + " " + t.Please + t.writeyourname + t.ibelow ),
            <.input(^.tpe := "text", ^.placeHolder := t.writeyourname + t.ihere)))
  }
}