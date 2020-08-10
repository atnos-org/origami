package org.atnos.origami

import cats._
import cats.syntax.either._
import org.atnos.origami.fold._

/**
 * List of predefined Folds
 */

object folds {

  import Fold.fractionalFold

  /** @return the average of all elements */
  def average[A](implicit f: Fractional[A]): Fold[Id, A, A] =
    plus[A] / count[A].map(f.fromInt)

  /** @return the average of all elements */
  def averageDouble[A](implicit n: Numeric[A]): Fold[Id, A, Double] =
    fractionalFold[Id, A, Double](plus[A].map(n.toDouble)) / count[A].map(_.toDouble)

  /** @return fold to count elements */
  def count[A]: FoldState[A, Int] =
    countOf(_ => true)

  /** @return fold to count elements */
  def countLong[A]: FoldState[A, Long] =
    countLongOf(_ => true)

  /** @return fold to count elements */
  def countOf[A](predicate: A => Boolean): FoldState[A, Int] =
    fromMonoidMap((a: A) => if (predicate(a)) 1 else 0)(IntAdditiveMonoid)

  /** @return fold to count elements */
  def countLongOf[A](predicate: A => Boolean): FoldState[A, Long] =
    fromMonoidMap((a: A) => if (predicate(a)) 1L else 0L)(LongAdditiveMonoid)

  /** @return fold to count the number of unique elements */
  def countUnique[A]: FoldId[A, Int] = new FoldId[A, Int] {
    type S = scala.collection.mutable.HashSet[A]

    def start = new scala.collection.mutable.HashSet[A]
    def fold = (s: S, a: A) => { s.add(a); s }
    def end(s: S) = s.size
  }

  /** @return return false if the list is empty or if all elements are false, use a Either state to indicate early success */
  def any[A](f: A => Boolean): FoldId[A, Boolean] = new FoldId[A, Boolean] {
    type S = Boolean Either Boolean

    def start = Either.left(false)
    def fold = (s: S, a: A) => if (f(a)) Either.right(true) else s
    def end(s: S) = s.fold(b => b, b => b)
  }

  /** @return return true if the list is empty or if all elements are true, use a Either state to indicate early failure */
  def all[A](f: A => Boolean): FoldId[A, Boolean] = new FoldId[A, Boolean] {
    type S = Boolean Either Boolean

    def start = Either.left(true)
    def fold = (s: S, a: A) => if (!f(a)) Either.right(false) else s
    def end(s: S) = s.fold(b => b, b => b)
  }

  /** @return the first element */
  def first[A]: FoldState[A, Option[A]] =
    fromFoldLeft[A, Option[A]](None)((u, a) => u.orElse(Option(a)))

  /** @return the last element */
  def last[A]: FoldState[A, Option[A]] =
    fromFoldLeft[A, Option[A]](None)((u, a) => Option(a))

  /** @return the first n elements */
  def firstN[A](n: Int): FoldId[A, List[A]] = new FoldId[A, List[A]] {
    type S = scala.collection.mutable.ListBuffer[A]

    def start = new scala.collection.mutable.ListBuffer[A]
    def fold = (s: S, a: A) => { if (s.size < n) s.append(a); s }
    def end(s: S) = s.toList
  }

  /** @return the last n elements */
  def lastN[A](n: Int): FoldId[A, List[A]] = new FoldId[A, List[A]] {
    type S = scala.collection.mutable.ListBuffer[A]

    def start = new scala.collection.mutable.ListBuffer[A]
    def fold = (s: S, a: A) => { s.append(a); if (s.size > n) s.remove(0); s }
    def end(s: S) = s.toList
  }

  /** @return the minimum element */
  def minimum[A : Ordering]: FoldState[A, Option[A]] = {
    val ordering = Ordering[A]
    fromFoldLeft[A, Option[A]](None)((u, a) => u.map(u1 =>
      ordering.min(a, u1)).orElse(Option(a)))
  }

  /** @return the minimum element or a default value */
  def minimumOr[A : Ordering](default: A): Fold[Id, A, A] =
    minimum[A].map(_.getOrElse(default))

  /** @return the minimum element */
  def maximum[A : Ordering]: FoldState[A, Option[A]] = {
    val ordering = Ordering[A]
    fromFoldLeft[A, Option[A]](None)((u, a) => u.map(u1 =>
      ordering.max(a, u1)).orElse(Option(a)))
  }

  /** @return the maximum element or a default value */
  def maximumOr[A : Ordering](default: A): Fold[Id, A, A] =
    maximum[A].map(_.getOrElse(default))

  /** @return the number of times an element changes its value */
  def flips[A]: FoldId[A, Int] = new FoldId[A, Int] {
    private var last: A = null.asInstanceOf[A]
    type S = Int

    def start = 0
    def fold = (s: S, a: A) =>
      if (last == null)   { last = a; s }
      else if (last != a) { last = a; s + 1 }
      else s
    def end(s: S) = s
  }

  /** @return the number of times an element changes its value */
  def flipsLong[A]: FoldId[A, Long] = new FoldId[A, Long] {
    private var last: A = null.asInstanceOf[A]
    type S = Long

    def start = 0L
    def fold = (s: S, a: A) =>
      if (last == null)   { last = a; s }
      else if (last != a) { last = a; s + 1L }
      else s
    def end(s: S) = s
  }

  /** @return the proportion of elements satisfying a given predicate */
  def proportion[A](predicate: A => Boolean): Fold[Id, A, Double] =
    (count[A] zip countOf(predicate)).map { case (total, passed) =>
      if (total == 0) 0.0
      else            passed.toDouble / total
    }

  /** @return gradient of a given variable A, compared to another V */
  def gradient[A : Numeric, V : Numeric]: FoldId[(A, V), Double] = new FoldId[(A, V), Double] {
    implicit val nt = implicitly[Numeric[A]]
    implicit val nv = implicitly[Numeric[V]]

    //       (count, sumx, sumy, sumyy, sumxy)
    type S = (Long, Long, Long, Long, Long)

    def start = (0L, 0L, 0L, 0L, 0L)
    def fold = (s: S, tv: (A, V)) => {
      val (a, v) = tv
      val (tl, vl) = (nt.toLong(a), nv.toLong(v))
      val (count, sumx, sumy, sumyy, sumxy) = s
      (count + 1,
        sumx + tl,
        sumy + vl,
        sumyy + (vl * vl),
        sumxy + (tl * vl)
        )
    }

    def end(s: S) = {
      val (count, sumx, sumy, sumyy, sumxy) = s
      val z = (sumyy * count) - (sumy * sumy)
      if (z == 0) 0.0
      else        ((sumxy * count) - (sumx * sumy)).toDouble / z
    }
  }

  /** constant fold returning the same value at the end */
  def const[A, B](b: =>B): FoldState[A, B] =
    last[A].as(b)

  /** lift a function to a fold that applies f to the last element */
  def lift[A, U](f: A => U): FoldState[A, Option[U]] =
    last[A] map ((_:Option[A]).map(f))

  /** @return a plus fold from a Num */
  def plus[N : Numeric]: FoldState[N, N] =
    fromFoldLeft(implicitly[Numeric[N]].zero)(implicitly[Numeric[N]].plus)

  /** @return a plus fold from a mapping to a Num */
  def plusBy[A, N : Numeric](f: A => N): Fold[Id, A, N] =
    plus[N].contramap[A](f)

  /** @return a times fold from a Num */
  def times[N : Numeric]: FoldState[N, N] =
    fromFoldLeft(implicitly[Numeric[N]].zero)(implicitly[Numeric[N]].times)

  /** @return a times fold from a mapping to a Num */
  def timesBy[A, N : Numeric](f: A => N): Fold[Id, A, N] =
    times[N].contramap(f)

  /** @return the mean of elements */
  def mean[N : Fractional]: Fold[Id, N, N]  =
    plus.zip(count).map { case (s, c) =>
      val frac = implicitly[Fractional[N]]; import frac._

      if (c == 0) frac.zero
      else        s / fromInt(c)
    }

  /** @return the number of elements, mean and standard deviation */
  def stddev[N : Fractional]: Fold[Id, N, Double] =
    onlineStddev.map(_._3)

  /** @return the number of elements, mean and standard deviation */
  def onlineStddev[N : Fractional]: Fold[Id, N, (Int, N, Double)] =
    onlineVariance map { case (count, mean, variation) =>
      implicit val num = implicitly[Fractional[N]]; import num._
      (count, mean, math.sqrt(toDouble(variation)))
    }

  /** @return the number of elements, mean and variance */
  def onlineVariance[N : Fractional]: Fold[Id, N, (Int, N, N)] =
    onlineVariation map { case (count, mean, variation) =>
      implicit val num = implicitly[Fractional[N]]; import num._

      if (count <= 1) (count, mean, variation)
      else            (count, mean, variation / fromInt(count))
    }

  /** @return the number of elements, mean and unbiased variance */
  def onlineUnbiasedVariance[N : Fractional]: Fold[Id, N, (Int, N, N)] =
    onlineVariation map { case (count, mean, variation) =>
      implicit val num = implicitly[Fractional[N]]; import num._

      if (count <= 1) (count, mean, variation)
      else            (count, mean, variation / fromInt(count - 1))
    }

  /** @return the number of elements, mean and variation */
  def onlineVariation[N : Fractional]: FoldId[N, (Int, N, N)] = new FoldId[N, (Int, N, N)] {
    implicit val num = implicitly[Fractional[N]]; import num._
    type S = (Int, N, N)

    def start = (0, num.zero, num.zero)

    def fold = (s: S, n: N) => {
      val (count, mean, variation) = s

      val count1 = count + 1
      val delta = n - mean
      val mean1 = mean + delta / fromInt(count1)
      val variation1 = variation + (delta * (n - mean1))

      (count1, mean1, variation1)
    }

    def end(s: S) = s
  }

  /** a fold where the current state is a random Int */
  def randomInt[A]: FoldId[A, Option[Int]] =
    randomWithGeneratorAndFunction[A, Int](new util.Random, (_:util.Random).nextInt)

  /** a fold where the current state is a random Int */
  def randomIntWithSeed[A](seed: Long): FoldId[A, Option[Int]] =
    randomWithGeneratorAndFunction[A, Int](new util.Random(seed), (_:util.Random).nextInt)

  /** a fold where the current state is a random Double */
  def randomDouble[A]: FoldId[A, Option[Double]] =
    randomWithGeneratorAndFunction[A, Double](new util.Random, (_:util.Random).nextDouble)

  /** a fold where the current state is a random Double */
  def randomDoubleWithSeed[A](seed: Long): FoldId[A, Option[Double]] =
    randomWithGeneratorAndFunction[A, Double](new util.Random(seed), (_:util.Random).nextDouble)

  /** create a fold for a mutable Random object */
  def randomWithGeneratorAndFunction[A, R](random: util.Random, f: util.Random => R): FoldId[A, Option[R]] = new FoldId[A, Option[R]] {
    type S = (util.Random, Option[R])

    def start = (random, None)
    def fold = (s: S, a: A) => { val r = f(s._1); (s._1, Option(r)) }
    def end(s: S) = Option(f(s._1))
  }

  /**
   * return an arbitrary streamed element so that each element has the same probability
   * be chosen
   */
  def reservoirSampling[A]: FoldId[A, Option[A]] = new FoldId[A, Option[A]] {
    type S = (scala.util.Random, Int, Option[A])

    def start = (new scala.util.Random, 0, None)

    def fold = (s: S, a: A) => {
      val (random, n, selected) = s
      val newSelection =
        selected match {
          case Some(a1) =>
            val r = random.nextInt(n + 1) + 1
            if (r == 1) Some(a1) else selected

          case None => Some(a)
        }
      (random, n + 1, newSelection)
    }

    def end(s: S) = s._3
  }

  /** @return a Fold which simply accumulates elements into a List */
  def list[A]: FoldId[A, List[A]] = new FoldId[A, List[A]] {
    // a ListBuffer is used for efficient appends
    type S = scala.collection.mutable.ListBuffer[A]

    def start = new scala.collection.mutable.ListBuffer[A]
    def fold = (s: S, a: A) => { s.append(a); s }
    def end(s: S) = s.toList
  }

  private val IntAdditiveMonoid: Monoid[Int] = new Monoid[Int] {
    def empty = 0
    def combine(a: Int, b: Int) = a + b
  }

  private val LongAdditiveMonoid: Monoid[Long] = new Monoid[Long] {
    def empty = 0L
    def combine(a: Long, b: Long) = a + b
  }
}
