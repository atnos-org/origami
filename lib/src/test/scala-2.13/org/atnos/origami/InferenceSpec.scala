package org.atnos
package origami

import org.specs2._
import folds._
import cats.implicits._

class InferenceSpec extends Specification { def is = s2"""

  issue #53 with existential types $e1
  another example for issue #53 $e2

"""

  def e1 = {
    (mean[Double].map(_ * 100), plus[Double]).tupled
    ok
  }

  def e2 = {
    folds.countUnique[String].zip(folds.count[String])
    ok
  }

}
