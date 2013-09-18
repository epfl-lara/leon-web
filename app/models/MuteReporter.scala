package leon.web
package models

import leon._

class MuteReporter() extends DefaultReporter(Settings()) {
  override def output(msg: String) = {}
}
