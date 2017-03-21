package org.atnos.origami

import cats.Monad
import cats.syntax.foldable._

package object syntax {

  object all extends FoldableFoldSyntax
  object foldable extends FoldableFoldSyntax

}
