package org.atnos.origami.addon.fs2

import org.atnos.origami.Fold

import fs2._
import cats._
import cats.syntax.functor._
import cats.syntax.flatMap._
import cats.effect._

package object stream { outer =>

  def scanEval[F[_] : Monad, S, A](p: Stream[F, A])(start: F[S])(f: (S, A) => F[S]): Stream[F, S] = {
    var state: S = null.asInstanceOf[S]
    def getState: S = state

    Stream.eval(start.map { s => state = s; s}) ++
    p.zip(Stream.constant(() => getState)).evalMap { case (a, s) =>
      f(s(), a).map { newState =>
        state = newState
        state
      }
    }
  }


  implicit class StreamSyntax[F[_] : Monad : Sync, A](p: Stream[F, A]) {

    def scanEval[S](start: F[S])(f: (S, A) => F[S]): Stream[F, S] =
      outer.scanEval(p)(start)(f)

    def foldWith[B](fold: Fold[F, A, B]): F[B] = {
      p.scanEval(fold.start)(fold.fold).compile.last.flatMap {
        case Some(s) => fold.end(s)
        case None    => fold.start.flatMap(fold.end)
      }
    }

  }

}
