package org.atnos.origami.addon.scalaz

import org.atnos.origami.Fold

import scalaz.stream._
import scalaz.{Catchable, Monad}
import scalaz.syntax.bind._

package object stream { outer =>

  def scanEval[F[_] : Monad, S, A](p: Process[F, A])(start: F[S])(f: (S, A) => F[S]): Process[F, S] = {
    var state: S = null.asInstanceOf[S]
    def getState: S = state

    Process.eval(start.map { s => state = s; s}) ++
    p.zip(Process.constant(() => getState)).evalMap { case (a, s) =>
      f(s(), a).map { newState =>
        state = newState
        state
      }
    }
  }


  implicit class ProcessSyntax[F[_] : Monad : Catchable, A](p: Process[F, A]) {

    def scanEval[S](start: F[S])(f: (S, A) => F[S]): Process[F, S] =
      outer.scanEval(p)(start)(f)

    def foldWith[B](fold: Fold[F, A, B]): F[B] = {
      p.scanEval(fold.start)(fold.fold).runLast.flatMap {
        case Some(s) => fold.end(s)
        case None    => fold.start.flatMap(fold.end)
      }
    }

  }

}
