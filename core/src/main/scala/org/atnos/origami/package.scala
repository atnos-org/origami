package org.atnos

import cats.Id

package object origami {

  object fold extends FoldCreation with FoldImplicits

  /** alias for a non-effectful Fold where the state type is U */
  type FoldState[A, B] = Fold[Id, A, B] { type S = B }

  /** alias for a Fold sinking its last value */
  type Sink[M[_], A] = Fold[M, A, Unit]

  /** alias for a Fold exposing it state type */
  type Aux[M[_], A, B, S1] = Fold[M, A, B] { type S = S1 }

}
