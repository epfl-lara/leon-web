/* Copyright 2009-2015 EPFL, Lausanne */

package leon.web
package client
package syntax

import monifu.reactive._

object observer {

  /** Provides a `!` operator to send message through an [[monifu.reactive.Observer]] */
  implicit class ObserverOps[A](val o: Observer[A]) extends AnyVal {
    def !(value: A) = o onNext value
  }

}

