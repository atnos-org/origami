package org.atnos.origami

import folds._
import cats._, data._
import cats.implicits._
import org.atnos.eff._, eff._
import org.atnos.eff.syntax.eff._
import org.scalacheck._, Prop._, Arbitrary._
import Arbitraries._

object FoldsSpec extends Properties("Folds") {

  property("count") = countFold
  property("countOf") = countOfFold
  property("count unique") = countUniqueFold

  property("any") = anyFold
  property("all") = allFold

  property("first") = firstFold
  property("last") = lastFold
  property("first n") = firstNFold
  property("last n") = lastNFold
  property("flips") = flipsFold
  property("proportion") = proportionFold
  property("gradient") = gradientFold

  property("plus") = plusFold
  property("plusBy") = plusByFold
  property("times") = timesFold
  property("timesBy") = timesByFold

  property("mean") = meanFold
  property("onlineVariance") = onlineVarianceFold
  property("from state") = stateFold

  property("random values") = randomFold
  property("reservoir sampling") = reservoirSamplingFold

  // see https://www.cs.ox.ac.uk/jeremy.gibbons/publications/iterator.pdf: "the essence of the Iterator pattern"
  property("line/word/char count") = lineWordCharCount

  type F[A, B] = Fold[NoFx, A, B]
  type FInt[A] = F[Int, A]

  def countFold = forAll { list: List[Int] =>
    count[Int].run(list).run ?= list.size
  }

  def countOfFold = forAll { list: List[Int] =>
    countOf[Int](isEven).run(list).run ?= list.count(isEven)
  }

  def countUniqueFold = forAll { list: List[Int] =>
    countUnique[Int].run(list).run ?= list.distinct.size
  }

  def anyFold = forAll { list: List[Boolean] =>
    folds.any[Boolean](identity _).run(list).run ?= list.exists(identity _)
  }

  def allFold = forAll { list: List[Boolean] =>
    folds.all[Boolean](identity _).run(list).run ?= list.forall(identity _)
  }

  def firstFold = forAll { list: List[Int] =>
    first[Int].run(list).run ?= list.headOption
  }

  def lastFold = forAll { list: List[Int] =>
    last[Int].run(list).run ?= list.lastOption
  }

  def firstNFold = forAll { (list: List[Int], n: SmallInt) =>
    firstN[Int](n.value).run(list).run ?= list.take(n.value)
  }

  def lastNFold = forAll { (list: List[Int], n: SmallInt) =>
    lastN[Int](n.value).run(list).run ?= list.takeRight(n.value)
  }

  def flipsFold = forAll { list: List[SmallInt] =>
    // fill a list of booleans with some n consecutive values where n is a small > 0 int
    // and values are sorted to avoid having 2 same consecutive same sizes
    // in that case the size of the list - 1 is the number of flips
    val consecutiveSizes = list.map(_.value).filter(_ > 0).sorted
    val booleans = consecutiveSizes.foldLeft((List[Boolean](), true)) { case ((res, b), cur) =>
      (res ++ List.fill(math.abs(cur))(b), !b)
    }._1

    val expected = if (consecutiveSizes.isEmpty) 0 else consecutiveSizes.size - 1
    flips[Boolean].run(booleans).run ?= expected
  }

  def proportionFold = forAll { list: List[Int] =>
    val p = proportion[Int](isEven).run(list).run
    if (list.isEmpty) p ?= 0.0
    else              p ?= list.filter(isEven).size.toDouble / list.size
  }

  def gradientFold = forAll { lists: List[(SmallInt, SmallInt)] =>
    val list = lists.map { case (x, y) => (x.value, y.value) }
    val g = gradient[Int, Int].run(list).run

    if (list.size <= 1) g ?= 0.0
    else {
      val meanx  = list.map(_._1).sum.toDouble / list.size
      val meany  = list.map(_._2).sum.toDouble / list.size
      val moment = list.map { case (x, y) => (x - meanx) * (y - meany) }.sum
      val square = list.map { case (x, y) => y.toDouble - meany }.map(y => y * y).sum
      if (square == 0) g ?= 0.0
      else             ((g - moment / square) <= 0.1) :| s"actual: $g, expected: ${moment/square}"
   }
  }

  def plusFold = forAll { list: List[Int] =>
    plus[Int].run(list).run ?= list.foldLeft(0)(_ + _)
  }

  def plusByFold = forAll { list: List[String] =>
    plusBy((_: String).size).run(list).run ?= list.foldLeft(0)(_ + _.size)
  }

  def timesByFold = forAll { list: List[String] =>
    timesBy((_: String).size).run(list).run ?= list.foldLeft(0)(_ * _.size)
  }

  def timesFold = forAll { list: List[Int] =>
    times[Int].run(list).run ?= list.foldLeft(0)(_ * _)
  }

  def meanFold = forAll { list: List[Double] =>
    if (list.isEmpty) mean[Double].run(list).run ?= 0.0
    else              mean[Double].run(list).run ?= list.sum / list.size
  }

  def onlineVarianceFold = forAll { nel: NonEmptyList[SmallDouble] =>
    val list = nel.toList.map(_.value)
    val count = list.size
    val mean  = list.sum / list.size
    val variance =
      if (count == 1) 0
      else            (list.map(x => x * x).sum - (list.sum * list.sum) / count) / count

    val (c, m, v) = onlineVariance[Double].run(list).run

    (c == count)     :| "count" &&
    (m ==~ mean)     :| "mean"  &&
    (v ==~ variance) :| "variance"
  }

  def lineWordCharCount = forAll { list1: List[String] =>
    val list = List("")
    val countChars: FoldId[Char, Int] = count[Char]
    val countWords: FoldId[Char, Int] = new Fold[NoFx, Char, Int] {
      type S = (Boolean, Int)
      def start = pure((false, 0))
      def fold = (s: S, t: Char) => {
        val (inWord, count) = s
        (t != ' ', if (!inWord && t != ' ') count + 1 else count)
      }
      def end(s: S) = pure(s._2)
    }
    val countLines = count[String]

    val countLinesWordsChars: FoldId[String, ((Int, Int), Int)] =
      countLines zip
      countWords.nest(_.toList) zip
      countChars.nest(_.toList)

    val words = list.flatMap(_.split(" ", -1)).filter(_.nonEmpty)
    val chars = words.flatMap(_.toList)

    countLinesWordsChars.run(list).run ?= (((list.size, words.size), chars.size))
  }

  def stateFold = forAll { list: List[Int] =>
    val stateFold: FoldId[Int, Option[Int]] = fold.fromStateEval((i: Int) => State[Int, Int] { count: Int =>
      val above10 = i >= 10
      val newCount = if (above10) count + 1 else count
      (newCount, newCount)
    })(0)

    stateFold.run(list).run.getOrElse(0) ?= list.count(_ > 10)
  }

  def randomFold = forAll { list: List[Int] =>
    val randomElements =
      randomInt[Int] compose folds.list

    val result = randomElements.run(list).run

    result.size ?= list.size
  }

  def reservoirSamplingFold = forAll { list1: List[Int] =>
    val list = list1.distinct
    val sampling = reservoirSampling[Int]
    val nSamples = (1 to list.size).map(_ => sampling.run(list).run)
    nSamples.flatten.distinct.size - list.size <= 2
  }

  /**
   * HELPERS
   */

  def isEven(i: Int): Boolean =
    i % 2 == 0

  /** use small doubles for online variance to avoid overflows */
  case class SmallDouble(value: Double) {
    override def toString = value.toString
  }

  implicit def ArbitrarySmallDouble: Arbitrary[SmallDouble] =
    Arbitrary(Gen.choose(-1000.0, 1000.0).map(SmallDouble))

  case class SmallInt(value: Int) {
    override def toString = value.toString
  }

  implicit def ArbitrarySmallInt: Arbitrary[SmallInt] =
    Arbitrary(Gen.choose(-1, 10).map(SmallInt))

}
