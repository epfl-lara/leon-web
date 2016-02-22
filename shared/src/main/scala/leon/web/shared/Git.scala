package leon.web
package shared

sealed abstract class GitOperation(val name: String)

object GitOperation {

  val STATUS = "status"
  val COMMIT = "commit"
  val PUSH   = "push"
  val PULL   = "pull"
  val RESET  = "reset"
  val LOG    = "log"

  case object Status extends GitOperation(STATUS)
  case object Push   extends GitOperation(PUSH)
  case object Pull   extends GitOperation(PULL)
  case object Reset  extends GitOperation(RESET)

  case class Commit(message: String) extends GitOperation(COMMIT)
  case class Log(number: Int)        extends GitOperation(LOG)
}

