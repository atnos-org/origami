package org.atnos.origami.addon.fs2

import org.atnos.origami.Fold
import fs2.Stream
import cats.syntax.functor._
import cats.syntax.flatMap._
import cats.effect.Sync
import cats.effect.concurrent.Ref

package object stream { outer =>

  def scanEval[F[_]: Sync, S, A](p: Stream[F, A])(start: F[S])(f: (S, A) => F[S]): Stream[F, S] = {

    def zipper(ref: Ref[F, S]): Stream[F, S] =
      p.zip(Stream.eval(ref.get).repeat).evalMap { case (a, s) =>
        for {
          ns <- f(s, a)
          _  <- ref.set(ns)
        } yield ns
      }

    for {
      st  <- Stream.eval(start)
      ref <- Stream.eval(Ref.of[F, S](st))
      rs  <- zipper(ref)
    } yield rs
  }


  implicit class StreamSyntax[F[_]: Sync, A](p: Stream[F, A]) {

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
