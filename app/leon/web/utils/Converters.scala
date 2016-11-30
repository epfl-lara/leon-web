package leon.web.utils

import leon.web.shared.StringPositionInSourceCode
import leon.utils.Positioned
import leon.utils.RangePosition

object Converters {
  def posToMessage(p: Positioned) = p.getPos match {
    case RangePosition(lineFrom, colFrom, pointFrom, lineTo, colTo, pointTo, file) => StringPositionInSourceCode(lineFrom, colFrom, lineTo, colTo)
    case _ => StringPositionInSourceCode(0, 0, 0, 0)
  }
}