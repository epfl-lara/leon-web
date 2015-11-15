/* Copyright 2009-2015 EPFL, Lausanne */

package leon.web.client
package syntax

import monifu.reactive._

/** Provides a `!` operator to send message through an [[monifu.reactive.Observer]] */
object Observer {

  implicit class ObserverSyntax[A](val o: Observer[A]) extends AnyVal {
    def !(value: A) = o.onNext(value)
  }

}

