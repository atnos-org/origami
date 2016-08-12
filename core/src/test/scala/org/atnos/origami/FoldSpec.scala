package org.atnos
package origami

import org.scalacheck.Arbitrary._
import org.scalacheck.Prop._
import org.scalacheck._
import Fold._
import cats._
import org.atnos.eff._
import org.atnos.eff.syntax.eff._
import cats.implicits._
import Arbitraries._

object FoldSpec extends Properties("Fold") {

  property("Folding a Foldable") = foldable

  property("Zip folds") = zip
  property("Compose folds") = compose

  property("from fold left") = fromFoldLeftProp
  property("from Monoid map") = fromMonoidMapProp

  def foldable = forAll { list: List[Int] =>
    listFold.run(list).run ?= list
  }

  def zip = forAll { (list: List[Int], fold1: FoldInt[String], fold2: FoldInt[String]) =>
    val fold = fold1 zip fold2

    fold.run(list) ?= fold.end(list.foldLeft(fold.start.run)(fold.fold))
  }

  def compose = forAll { (list: List[Int], fold: FoldInt[Int]) =>
    val scans = fold compose listFold

    // scans is *not* equivalent to a scanLeft because it leaves out the start element
    scans.run(list) == list.scanLeft(fold.start.run)(fold.fold).drop(1).traverseU(fold.end)
  }

  def fromFoldLeftProp = forAll { list: List[String] =>
    val fold: FoldString[Int] =
      fromFoldLeft[NoFx, String, Int](0)((s, t) => s + t.size)

    fold.run(list).run ?= list.foldLeft(0)(_ + _.size)
  }

  def fromMonoidMapProp = forAll { list: List[String] =>
    val fold: FoldString[Int] =
      fromMonoidMap[NoFx, String, Int](_.size)

    fold.run(list).run ?= list.foldLeft(0)(_ + _.size)
  }

  /**
   * HELPERS
   */

  def listFold = Fold.fromFoldLeft[NoFx, Int, List[Int]](List[Int]())(_ :+ _)

}
