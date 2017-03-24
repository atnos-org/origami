package org.atnos
package origami

import org.scalacheck.Arbitrary._
import org.scalacheck.Prop._
import org.scalacheck._
import fold._
import cats._
import cats.implicits._
import Arbitraries._
import scala.concurrent._, duration._
import scala.concurrent.ExecutionContext.Implicits.global

object FoldSpec extends Properties("Fold") {

  property("Folding a Foldable") = foldable

  property("Zip folds") = zip
  property("Compose folds") = compose
  property("Combine folds") = combine

  property("from fold left") = fromFoldLeftProp
  property("from Monoid map") = fromMonoidMapProp

  property("a FoldId can be 'injected' into a monadic fold") = toMonadicFold

  def foldable = forAll { list: List[Int] =>
    listFold.run(list) ?= list
  }

  def zip = forAll { (list: List[Int], fold1: FoldInt[String], fold2: FoldInt[String]) =>
    val fold = fold1 zip fold2

    fold.run(list) ?= fold.end(list.foldLeft(fold.start)(fold.fold))
  }

  def compose = forAll { (list: List[Int], fold: FoldInt[Int]) =>
    val scans = fold compose listFold

    // scans is *not* equivalent to a scanLeft because it leaves out the start element
    scans.run(list) == list.scanLeft(fold.start)(fold.fold).drop(1).traverseU(fold.end)
  }

  def combine = forAll { (list: List[Int], fold1: FoldInt[Int], fold2: FoldInt[Int], fold3: FoldInt[Int]) =>
    val combined = List(fold1, fold2, fold3).combineAll

    combined.run(list) === List(fold1, fold2, fold3).map(_ run list).combineAll
  }

  def fromFoldLeftProp = forAll { list: List[String] =>
    val fold: FoldString[Int] =
      fromFoldLeft[String, Int](0)((s, t) => s + t.size)

    fold.run(list) ?= list.foldLeft(0)(_ + _.size)
  }

  def fromMonoidMapProp = forAll { list: List[String] =>
    val fold: FoldString[Int] =
      fromMonoidMap[String, Int](_.size)

    fold.run(list) ?= list.foldLeft(0)(_ + _.size)
  }

  def toMonadicFold = forAll { list: List[String] =>
    val fold: FoldString[Int] =
      fromMonoidMap[String, Int](_.size)

    val monadicFold = fold.into[Future]

    Await.result(monadicFold.run(list), 1.second) ?= list.foldLeft(0)(_ + _.size)
  }

  /**
   * HELPERS
   */

  def listFold = fromFoldLeft[Int, List[Int]](List[Int]())(_ :+ _)

}
