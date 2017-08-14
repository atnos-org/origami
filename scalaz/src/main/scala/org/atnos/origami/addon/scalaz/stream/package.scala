package org.atnos.origami.addon.scalaz

import org.atnos.origami.Fold

import scalaz.stream._
import scalaz.{Catchable, Monad}
import scalaz.syntax.bind._

package object stream { outer =>

  def scanEval[F[_] : Monad : Catchable, S, A](p: Process[F, A])(start: F[S])(f: (S, A) => F[S]): Process[F, S] = {

    def go(s: S, p0: Process[F, A]): Process[F, S] =
      for {
        pairOpt <- Process.eval(p0.unconsOption)
        pair    <- pairOpt.fold[Process[F, (A, Process[F, A])]](Process.halt)(Process.emit)
        s0      <- Process.eval(f(s, pair._1))
        s1      <- Process.emit(s0) ++ go(s0, pair._2)
      } yield s1

    for {
      s0 <- Process.eval(start)
      s1 <- go(s0, p)
    } yield s1
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
