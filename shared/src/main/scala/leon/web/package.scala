package leon

import leon.web.shared.equal

package object web extends equal.EqSyntax  {

  type Eq[A, B] = equal.Eq[A, B]

  val Eq = equal.Eq

}

