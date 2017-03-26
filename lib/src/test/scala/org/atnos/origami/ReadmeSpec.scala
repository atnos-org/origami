package org.atnos.origami

import java.io.PrintWriter

import cats.Eval
import org.specs2.Specification

class ReadmeSpec extends Specification { def is = s2""""

 We can compute the minimum, maximum, average of a list of numbers while writing each element to a file $e1

"""

  def e1 = {
    import org.atnos.origami.fold._
    import org.atnos.origami.folds._
    import org.atnos.origami.syntax.foldable._
    import cats.data._
    import cats.implicits._

    type Safe[A] = EitherT[Eval, Throwable, A]
    def protect[A](a: =>A): Safe[A] = EitherT.right(Eval.later(a))

    def saveToFile(path: String): Sink[Safe, Int] =
      bracket(
        protect(new PrintWriter(path)))(
        (w, i: Int) => protect { w.write(s"i=$i\n"); w })(
        w => protect(w.close))

    val stats: Fold[Safe, Int, ((Int, Int), Double)] =
      (minimumOr(0) <*> maximumOr(Int.MaxValue) <*> averageDouble).into[Safe] <*
        saveToFile("target/readme-example")


    (0 to 10).toList.foldWith(stats).value.value ==== Right(((0, 10), 5))
  }


}
