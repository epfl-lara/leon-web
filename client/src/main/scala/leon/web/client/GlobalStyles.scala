package leon.web
package client

import scalacss.Defaults._

import scala.language.postfixOps

object GlobalStyles extends StyleSheet.Inline {
  import dsl._

  /*style(unsafeRoot("body")(
    paddingTop(50.px))
  )*/

  //val bootstrapStyles = new BootstrapStyles
  
  /*val programBox = style(
     width(400 px),
     height(600 px),
     display.inlineBlock
  )
  val codebox = style(
     width(100%%),
     height(300 px),
     display.inlineBlock
  )*/
  val discussionbox = style(
     width :=! "calc(100% - 30px)",
     maxWidth(600 px)
  )
  
  val discussionComment = style(
     width :=! "calc(100% - 110px)",
     display.inlineBlock,
     verticalAlign.top,
     marginLeft(10 px)
  )
  /*val website = style(
     width(400 px),
     height(600 px),
     display.inlineBlock,
     verticalAlign.top
  )*/
  
  val pseudoMixin = mixin(
      content := "\"\"",
      position.absolute,
      borderStyle.solid,
      display.block,
      width(0 px)
  )
  
  val borderComment = c"#6C5D53"
  
  val triangleBorder = style(
    position.relative,
    padding(15 px),
    margin(1 em, 0 em, 1 em),
    border(1 px,solid,borderComment),
    color(c"#333"),
    backgroundColor(c"#fff"),
    borderRadius(10 px),
    &.before(
      pseudoMixin,
      bottom(-20 px),
      right(44 px),
      borderWidth(20 px,20 px, 0 px),
      borderColor(borderComment, transparent)
    ),
    &.after(
      pseudoMixin,
      bottom(-13 px),
      right(50 px),
      borderWidth(13 px,13 px, 0 px),
      borderColor(c"#fff", transparent)
    )
  )
  val triangleBorderRight = style(
    triangleBorder,
    marginRight(30 px),
    &.before(
      top(10 px),
      bottom.auto,
      left.auto,
      right(-30 px),
      borderWidth(15 px, 0 px, 15 px, 30 px),
      borderColor(transparent, borderComment)
    ),
    &.after(
      top(11 px),
      bottom.auto,
      left.auto,
      right(-27 px),
      borderWidth(14 px, 0 px, 14 px, 27 px),
      borderColor(transparent, c"#fff")
    )
  )
  
  val webbuilderClarificationAddition = style(
      backgroundColor(c"#0f0")
  )
  val webbuilderClarificationDeletion = style(
      backgroundColor(c"#f00")
  )
  val webbuilderClarificationModification = style(
      backgroundColor(c"#ff0")
  )
}
