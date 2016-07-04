package leon.web
package client
package websitebuilder

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._
import org.scalajs.jquery.{ jQuery => $, _ }

object DiffViewer {
  def displayDiff(s1: String, s2: String, maxDisplay: Int,
      classPositive: String, classNegative: String, classChange: String,
      s1OnClick: => Unit, s2OnClick: => Unit
    ): (ReactTagOf[org.scalajs.dom.html.Div], ReactTagOf[org.scalajs.dom.html.Div]) = {
    val preLength = longestPrefixLength(s1, s2)
    val prefix = s1.substring(0, preLength)
    val s1next = s1.substring(preLength)
    val s2next = s2.substring(preLength)
    val postLength = longestSuffixLength(s1next, s2next)
    
    val s1mid = s1next.substring(0, s1next.length - postLength)
    val s2mid = s2next.substring(0, s2next.length - postLength)
    val suffix = s1next.substring(s1next.length - postLength)

    val s1mid_stripped = insertEllipsisIfNeeded(s1mid, maxDisplay - Math.min(6, preLength + postLength))
    val s2mid_stripped = insertEllipsisIfNeeded(s2mid, maxDisplay - Math.min(6, preLength + postLength))
    
    val (s1midLength, s2midLength) = (s1mid_stripped.length, s2mid_stripped.length)
    val maxSize = Math.max(s1midLength, s2midLength)
    val remaining = maxDisplay - maxSize
    val halfRemaining = remaining / 2
    
    // Now we compute lengths. Priority to display the inner elements
    val (prefixLength, suffixLength) = if(preLength <= halfRemaining) {
      (preLength, remaining - preLength)
    } else if(postLength <= halfRemaining) {
      (remaining - postLength, postLength)
    } else {
      (halfRemaining, remaining - halfRemaining)
    }
        
    val classMid = if(preLength + postLength == s1.length) { // This is an addition
      classPositive
    } else if(preLength + postLength == s2.length) { // This is a deletion
      classNegative
    } else { // This is a modification
      classChange
    }
    
    val prefix_stripped = preInsertEllipsisIfNeeded(prefix, prefixLength)
    val suffix_stripped = postInsertEllipsisIfNeeded(suffix, suffixLength)
    
    (
        <.div(
          <.span(prefix_stripped),
          <.span(^.className := classMid, s1mid_stripped),
          <.span(suffix_stripped),
          ^.onClick --> Callback{s1OnClick}
        ),
        <.div(
          <.span(prefix_stripped),
          <.span(^.className := classMid, s2mid_stripped),
          <.span(suffix_stripped),
          ^.onClick --> Callback{s2OnClick}
        )
    )
  }
  
  case class FullDiff(original: String, prefix: String, middle: JQuery, suffix: String)
  
  
  def displayDiffs(s1: String, alternatives: List[String], maxDisplay: Int,
      classPositive: String, classNegative: String, classChange: String,
      s1OnClick: () => Unit, otherCallbacks: List[() => Unit],
      s1OnMouseEnter: FullDiff => () => Unit = _ => () => (), otherOnMouseEnter: List[FullDiff => () => Unit] = Nil,
      s1OnMouseLeave: FullDiff => () => Unit = _ => () => (), otherOnMouseLeave: List[FullDiff => () => Unit] = Nil
    ): List[ReactTagOf[org.scalajs.dom.html.Div]] = {
    val preLength = alternatives.map(s2 => longestPrefixLength(s1, s2)).min
    val prefix = s1.substring(0, preLength)
    val s1next = s1.substring(preLength)
    val s2nexts = alternatives.map(s2 => s2.substring(preLength))
    val postLength = s2nexts.map(s2next => longestSuffixLength(s1next, s2next)).min
    
    val s1mid = s1next.substring(0, s1next.length - postLength)
    val s2mids = s2nexts.map(s2next => s2next.substring(0, s2next.length - postLength))
    val suffix = s1next.substring(s1next.length - postLength)

    val s1mid_stripped = insertEllipsisIfNeeded(s1mid, maxDisplay - Math.min(6, preLength + postLength))
    val s2mids_stripped = s2mids.map(s2mid =>
      insertEllipsisIfNeeded(s2mid, maxDisplay - Math.min(6, preLength + postLength)))
    
    val (s1midLength, s2midLengths) = (s1mid_stripped.length, s2mids_stripped.map(_.length))
    val maxSize = Math.max(s1midLength, s2midLengths.max)
    val remaining = maxDisplay - maxSize
    val halfRemaining = remaining / 2
    
    // Now we compute lengths. Priority to display the inner elements
    val (prefixLength, suffixLength) = if(preLength <= halfRemaining) {
      (preLength, remaining - preLength)
    } else if(postLength <= halfRemaining) {
      (remaining - postLength, postLength)
    } else {
      (halfRemaining, remaining - halfRemaining)
    }
    
    val classMid = if(preLength + postLength == s1.length) { // This is an addition
      classPositive
    } else if(alternatives.forall(s2 => preLength + postLength == s2.length)) { // This is a deletion
      classNegative
    } else { // This is a modification
      classChange
    }

    val prefix_stripped = preInsertEllipsisIfNeeded(prefix, prefixLength)
    val suffix_stripped = postInsertEllipsisIfNeeded(suffix, suffixLength)
    
    val s1FullDiff = FullDiff(s1, prefix, $("<span>").text(s1mid).addClass(classMid), suffix)
    val s1OnMouseEnterC = s1OnMouseEnter(s1FullDiff)
    val s1OnMouseLeaveC = s1OnMouseLeave(s1FullDiff)
    (<.div(
      ^.className := "btn btn-default",
      <.span(prefix_stripped),
      <.span(^.className := classMid, s1mid_stripped),
      <.span(suffix_stripped),
      ^.onClick --> Callback{s1OnClick()},
      ^.onMouseEnter --> Callback{s1OnMouseEnterC()},
      ^.onMouseLeave --> Callback{s1OnMouseLeaveC()}
    ) ::
    (s2mids_stripped.zipWithIndex.map{ case (s2mid_stripped, index) =>
      val s2OnClick = otherCallbacks.applyOrElse(index, (_: Int) => () => ())
      val s2mid = s2mids.applyOrElse(index, (_: Int) => "")
      val fullDiff = FullDiff(s1, prefix, $("<span>").text(s2mid).addClass(classMid), suffix)
      val s2OnMouseEnter = otherOnMouseEnter.applyOrElse(index, (_: Int) => (_: FullDiff) => () => ())(fullDiff)
      val s2OnMouseLeave = otherOnMouseLeave.applyOrElse(index, (_: Int) => (_: FullDiff) => () => ())(fullDiff)
      
      <.div(
        ^.className := "btn btn-default",
        <.span(prefix_stripped),
        <.span(^.className := classMid, s2mid_stripped),
        <.span(suffix_stripped),
        ^.onClick --> Callback{s2OnClick()},
        ^.onMouseEnter --> Callback{s2OnMouseEnter()},
        ^.onMouseLeave --> Callback{s2OnMouseLeave()}
      )
    }))
  }
  
  private def longestPrefixLength(a: String, b: String): Int = {
    var i = 0
    while(i < a.length && i < b.length && a(i) == b(i)) {
      i += 1
    }
    i
  }
  private def longestSuffixLength(a: String, b: String): Int = {
    var i = 0
    while(i < a.length && i < b.length && a(a.length - i - 1) == b(b.length - i - 1)) {
      i += 1
    }
    i
  }
  //Shortens a string to the given size. Inserts ... in the middle if necessary.
  private def insertEllipsisIfNeeded(a: String, size: Int): String = {
    if(a.length <= size) a else {
      val c = size/2
      a.substring(0, c) + "…" + a.substring(a.length - (size - c + 1), a.length)
    }
  }
  //Shortens a string to the given size. Inserts ... at the beginning if necessary.
  private def preInsertEllipsisIfNeeded(a: String, size: Int): String = {
    if(a.length <= size) a else {
      "…" + a.substring(a.length - size + 1)
    }
  }
  //Shortens a string to the given size. Inserts ... at the end if necessary.
  private def postInsertEllipsisIfNeeded(a: String, size: Int): String = {
    if(a.length <= size) a else {
      a.substring(0, size - 1) + "…"
    }
  }
}