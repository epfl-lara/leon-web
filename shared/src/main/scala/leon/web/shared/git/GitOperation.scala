package leon.web.shared.git

sealed trait GitOperation {
  def name: String
}

case object GitStatus extends GitOperation {
  val name = GitOperation.STATUS
}
case object GitPull  extends GitOperation {
  val name = GitOperation.PULL
}
case object GitReset  extends GitOperation {
  val name = GitOperation.RESET
}

case class GitCommit(message: String) extends GitOperation {
  val name = GitOperation.COMMIT
}
case class GitPush(force: Boolean)    extends GitOperation {
  val name = GitOperation.PUSH
}
case class GitLog(number: Int)        extends GitOperation {
  val name = GitOperation.LOG
}


object GitOperation {
  val STATUS = "status"
  val COMMIT = "commit"
  val PUSH   = "push"
  val PULL   = "pull"
  val RESET  = "reset"
  val LOG    = "log"
}
