package leon.web.client
package syntax

import monifu.reactive.Subject

object subject {

  implicit class SubjectOps[I, +T](val subject: Subject[I, T]) extends AnyVal {
    def !(event: I): Unit = subject.onNext(event)
  }

}

