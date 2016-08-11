package org.atnos
package origami

import org.scalacheck.Arbitrary._
import org.scalacheck.Prop._
import org.scalacheck._
import Fold._
import FoldId._
import Arbitraries._
import cats._
import org.atnos.eff._
import org.atnos.eff.syntax.eff._
import cats.implicits._

object FoldSpec extends Properties("Fold") {

  property("Folding a Foldable") = foldable
//  property("Folding an Iterator") = iterator
//
//  property("Zip folds") = zip
//  property("Compose folds") = compose
//
//  property("Apply laws") = applyLaws
//  property("Compose laws") = composeLaws
//  property("Cobind laws") = cobindLaws
//
//  property("from fold left") = fromFoldLeftProp
//  property("from Monoid map") = fromMonoidMapProp
//  property("from Reducer") = fromReducerProp
//
//  property("stop iteration early") = breakProp

  def foldable = forAll { list: List[Int] =>
    FoldId.list[NoFx, Int].run(list).run === list
  }

  /*
  def iterator = forAll { (list: List[Int], fold: F[Int, String]) =>
    fold.run(list.iterator) == runFoldOnList(list, fold)
  }

  def zip = forAll { (list: List[Int], fold1: F[Int, String], fold2: F[Int, String]) =>
    implicit val app = Apply[FInt]
    val fold = fold1 zip fold2

    fold.run(list.iterator) == fold.end(list.foldLeft(fold.start)(fold.fold))
  }

  def compose = forAll { (list: List[Int], fold: F[Int, Int]) =>    
    val scans: F[Int, List[Int]] = fold compose Folds$.list

    // scans is *not* equivalent to a scanLeft because it leaves out the start element
    scans.run(list.iterator) == list.scanLeft(fold.start)(fold.fold).drop(1).traverseU(fold.end)
  }

  /**
   * Laws
   */
  def applyLaws = forAll { (fbc: F[Int, String => Int], fab: F[Int, Int => String], fa: F[Int, Int]) =>
    Apply[FInt].applyLaw.composition(fbc, fab, fa)
  }

  def composeLaws = forAll { (fa: F[Int, String], fb: F[String, Int], fc: F[Int, String]) =>
    Compose[F].composeLaw.associative(fa, fb, fc)
  }

  def cobindLaws = forAll { (fa: FInt[Int], f: FInt[Int] => String, g: FInt[String] => Int, h: FInt[Int] => String) =>
    Cobind[FInt].cobindLaw.cobindAssociative(fa, f, g, h)
  }

  def fromFoldLeftProp = forAll { list: List[String] =>
    val fold: Fold[String, Int] =
      fromFoldLeft[String, Int](0)((s, t) => s + t.size)

    fold.run(list) ?= list.foldLeft(0)(_ + _.size)
  }

  def fromMonoidMapProp = forAll { list: List[String] =>
    val fold: Fold[String, Int] =
      fromMonoidMap[String, Int](_.size)

    fold.run(list) ?= list.foldLeft(0)(_ + _.size)
  }

  def fromReducerProp = forAll { list: List[String] =>
    val fold: Fold[String, Int] =
      fromReducer(Reducer.unitReducer[String, Int](_.size))

    fold.run(list) ?= list.foldLeft(0)(_ + _.size)
  }

  def breakProp = forAllNoShrink { (list: List[Int], maxValue: NaturalIntSmall) =>
    val max = maxValue.value
    
    val breakableCount = count[Int].breakWhen(n => n >= max)
    implicit val v: FoldableM[Id, List[Int], Int] = FoldableIsFoldableM[Id, List, Int]

    val result = breakableCount.runBreak(list)

    (list.size <= max && result == list.size) :| ("max not reached: "+((result, list, max))) ||
    (list.size > max  && result == max)       :| ("max reached: "+((result, list, max)))
  }

  /**
   * HELPERS
   */
  def runFoldOnList[A](list: List[Int], fold: F[Int, A]): Id[A] =
    fold.start.flatMap { i: fold.S =>
      fold.end(list.foldLeft(i: fold.S)((res, cur) => fold.fold(res, cur)))
    }

  def runScanOnList[A](list: List[Int], fold: F[Int, A]): List[A] =
    fold.start.flatMap { i: fold.S =>
      list.scanLeft(i: fold.S)((res, cur) => fold.fold(res, cur)).traverseU(fold.end)
    }
*/
}
