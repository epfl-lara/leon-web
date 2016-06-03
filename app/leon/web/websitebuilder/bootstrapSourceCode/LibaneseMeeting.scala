import leon.collection._
import leon.webDSL.webDescription._
import leon.webDSL.webBuilding._
import implicits._

object Main {
  val mealOfTheDay = Meal("Kafta Bel Sanieh", "Galette de viande de boeuf hachee et persillee avec quartier de pomme de terre fondante cuits au four sauce tomate", false)
  val sandwich = "Sandwich"

  case class Meal(title: String, description: String, vegetarian: Boolean) {
    def mkString() = title + " (" + description + ")" +
      (if(vegetarian) " - Vegetarian" else "")
  }
  val tcheck = ^.tpe := "checkbox"
  def tradio = ^.tpe := "radio"
   
  val sandwiches = List(
    Meal("Chawarma Djej", "chicken, pickle, garlic sauce", false),
    Meal("Chawarma Lahme", "roasted meat, pickle, oignon, tomato, parsley, tarator sauce", false),
    Meal("Tawouk", "roasted white of chicken", false),
    Meal("Kafta", " minced meat, oignons, parsley", false),
    Meal("Falafel", "crispy chickpeas, beans, pickles, tarator sauce", true),
    Meal("Labne", "salted white cheese, tomato, mint, olives", true)
  )
  
  val plateauDescription = "Hommos, Tabboule, Moutabbal & Bread"
  
  val vegetarianPlateau = "Vegetarian plateau composed of " + plateauDescription + " + Sambousek Bel Jebneh, Falafel, Fatayer"
  
  def makeList(l: List[String]): String = l match {
    case Cons(a, b: Cons[String]) => a + ", " + makeList(b)
    case Nil() => ""
    case Cons(a, Nil()) => a
  }

  //This function is called by the evaluator. It has to return a WebPage
  def main() = {
    val mainChoice = "MainMeal"
    val plateauId = "plateau"
    val page = 
    <.div(
      <.h2("Lara Conference around Libanese food", ^.color := "#5E68F7"),
      "Your name: ", <.input(
        ^.tpe := "text",
        ^.id := "name",
        ^.placeHolder := "Your name here"),
      <.p("Please make your choice between the following meals:"),
      <.div(
        "1.",
        <.label(
          <.input(tradio,
            ^.name := mainChoice,
            ^.value := mealOfTheDay.title
          ), "Meal of the day: " + mealOfTheDay.mkString)
      ),
      <.div("2. Choose a sandwich, and optionally the plateau")(
        sandwiches map { s =>
          <.div(
            <.label(<.input(
              tradio,
              ^.name := mainChoice,
              ^.id := s.title,
              ^.value := s.title),
            "Sandwich " + s.mkString))
          : WebTree
        }
      )(
        <.label(
          <.input(tcheck, ^.id := plateauId),
          " with the plateau ("+plateauDescription+")",
          CssAcceptor[String]("marginLeft") := "2em"
          )),
      <.div(
        "3.",
        <.label(
          <.input(tradio,
            ^.name := mainChoice,
            ^.value := "Plateau"
          ), vegetarianPlateau)
      ),
      <.input(
        ^.tpe := "submit",
        ^.value := "That's what I want")
    )
    WebPage(page)
  }
}
