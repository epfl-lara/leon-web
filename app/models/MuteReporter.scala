package leon.web
package models

import leon._

class MuteReporter() extends DefaultReporter(Set()) {
  override def emit(msg: Message) = {}
}
