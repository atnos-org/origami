package org.atnos.origami.syntax

import cats._
import cats.syntax.all._
import org.atnos.origami._

trait FoldableFoldSyntax {

  implicit class FoldableFoldOps[F[_] : Foldable, A](foldable: F[A]) {
    def foldWith[M[_] : Monad, B](f: Fold[M, A, B]): M[B] =
      f.start.flatMap { s =>
        Foldable[F].foldM(foldable, s)((s1: f.S, a: A) => f.fold(s1, a)).flatMap(f.end)
      }
  }

}

object FoldableFoldSyntax extends FoldableFoldSyntax

