package org.atnos.origami

import org.scalacheck.Prop.forAll
import org.scalacheck.Properties
import org.atnos.origami.syntax.foldable._
import folds._
import cats.implicits._

class FoldableSpec extends Properties("Foldable") {

  property("cats foldable can be folded with an origami fold") = foldableOrigami

  def foldableOrigami = forAll { list: List[String] =>
    list.foldWith(count) === list.size
  }

}
