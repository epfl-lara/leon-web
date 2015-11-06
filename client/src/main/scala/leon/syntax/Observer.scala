package leon.web.client
package syntax

import monifu.reactive._

object Observer {

  implicit class ObserverSyntax[A](val o: Observer[A]) extends AnyVal {
    def !(value: A) = o.onNext(value)
  }

}

