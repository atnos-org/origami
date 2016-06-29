package org.atnos
package origami

import java.security.MessageDigest
import cats._, data.Xor
import FoldEff._
import org.atnos.eff.{Fold =>_,_}, eff._

/**
 * List of predefined Folds
 */
trait Folds {

  private val IntAdditiveMonoid = new Monoid[Int] {
    def empty = 0
    def combine(a: Int, b: Int) = a + b
  }

  private val LongAdditiveMonoid = new Monoid[Long] {
    def empty = 0L
    def combine(a: Long, b: Long) = a + b
  }

  /** @return fold to count elements */
  def count[T]: FoldState[T, Int] =
    countOf(_ => true)

  /** @return fold to count elements */
  def countLong[T]: FoldState[T, Long] =
    countLongOf(_ => true)

  /** @return fold to count elements */
  def countOf[T](predicate: T => Boolean): FoldState[T, Int] =
    fromMonoidMap((t: T) => if (predicate(t)) 1 else 0)(IntAdditiveMonoid)

  /** @return fold to count elements */
  def countLongOf[T](predicate: T => Boolean): FoldState[T, Long] =
    fromMonoidMap((t: T) => if (predicate(t)) 1L else 0L)(LongAdditiveMonoid)

  /** @return a Fold which simply accumulates elements into a List */
  def list[R, T]: FoldEff[R, T, List[T]] = new FoldEff[R, T, List[T]] {
    // a ListBuffer is used for efficient appends
    type S = scala.collection.mutable.ListBuffer[T]
    def start = pure(new scala.collection.mutable.ListBuffer[T])
    def fold = (s: S, t: T) => { s.append(t); s }
    def end(s: S) = pure(s.toList)
  }

  /*
  /** @return fold to count the number of unique elements */
  def countUnique[T]: Fold[T, Int] = new Fold[T, Int] {
    type S = scala.collection.mutable.HashSet[T]
    def start = new scala.collection.mutable.HashSet[T]
    def fold = (s: S, t: T) => { s.add(t); s }
    def end(s: S) = s.size
  }

  /** @return return false if the list is empty or if all elements are false, use a Xor.right state to indicate early success */
  def any[T](f: T => Boolean) = new Fold[T, Boolean] {
    type S = Boolean Xor Boolean
    def start = Xor.left(false)
    def fold = (s: S, t: T) => if (f(t)) Xor.right(true) else s
    def end(s: S) = s.fold(b => b, b => b)
  }

  /** @return return true if the list is empty or if all elements are true, use a Xor.right state to indicate early failure */
  def all[T](f: T => Boolean) = new Fold[T, Boolean] {
    type S = Boolean Xor Boolean
    def start = Xor.left(true)
    def fold = (s: S, t: T) => if (!f(t)) Xor.right(false) else s
    def end(s: S) = s.fold(b => b, b => b)
  }

  /** @return the first element */
  def first[T]: FoldState[T, Option[T]] =
    fromFoldLeft[T, Option[T]](None)((u, t) => u.orElse(Some(t)))

  /** @return the last element */
  def last[T]: FoldState[T, Option[T]] =
    fromFoldLeft[T, Option[T]](None)((u, t) => Some(t))

  /** @return the first n elements */
  def firstN[T](n: Int) = new Fold[T, List[T]] {
    type S = scala.collection.mutable.ListBuffer[T]
    def start = new scala.collection.mutable.ListBuffer[T]
    def fold = (s: S, t: T) => { if (s.size < n) s.append(t); s }
    def end(s: S) = s.toList
  }

  /** @return the last n elements */
  def lastN[T](n: Int) = new Fold[T, List[T]] {
    type S = scala.collection.mutable.ListBuffer[T]
    def start = new scala.collection.mutable.ListBuffer[T]
    def fold = (s: S, t: T) => { s.append(t); if (s.size > n) s.remove(0); s }
    def end(s: S) = s.toList
  }

  /** @return the number of times an element changes its value */
  def flips[T] = new Fold[T, Int] {
    private var last: T = null.asInstanceOf[T]
    type S = Int
    def start = 0
    def fold = (s: S, t: T) =>
      if (last == null)   { last = t; s }
      else if (last != t) { last = t; s + 1 }
      else s
    def end(s: S) = s
  }

  /** @return the number of times an element changes its value */
  def flipsLong[T] = new Fold[T, Long] {
    private var last: T = null.asInstanceOf[T]
    type S = Long
    def start = 0L
    def fold = (s: S, t: T) =>
      if (last == null)   { last = t; s }
      else if (last != t) { last = t; s + 1L }
      else s
    def end(s: S) = s
  }

  /** @return the proportion of elements satisfying a given predicate */
  def proportion[T](predicate: T => Boolean): Fold[T, Double] =
    (count[T] zip countOf(predicate)).map { case (total, passed) =>
      if (total == 0) 0.0
      else            passed.toDouble / total
    }

  /** @return gradient of a given variable T, compared to another V */
  def gradient[T : Numeric, V : Numeric]: Fold[(T, V), Double] = new Fold[(T, V), Double] {
    implicit val nt = implicitly[Numeric[T]]
    implicit val nv = implicitly[Numeric[V]]

    //       (count, sumx, sumy, sumyy, sumxy)
    type S = (Long, Long, Long, Long, Long)
    def start = (0L, 0L, 0L, 0L, 0L)
    def fold = (s: S, tv: (T, V)) => {
      val (t, v) = tv
      val (tl, vl) = (nt.toLong(t), nv.toLong(v))
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

  /** lift a function to a fold that applies f to the last element */
  def lift[T, U](f: T => U) =
    last[T] map ((_:Option[T]).map(f))

  /** @return a plus fold from a Num */
  def plus[N : Numeric]: FoldState[N, N] =
    fromFoldLeft(implicitly[Numeric[N]].zero)(implicitly[Numeric[N]].plus)

  /** @return a plus fold from a mapping to a Num */
  def plusBy[A, N : Numeric](f: A => N): FoldState[A, N] =
    plus[N].contramap[A](f)

  /** @return a times fold from a Num */
  def times[N : Numeric]: FoldState[N, N] =
    fromFoldLeft(implicitly[Numeric[N]].zero)(implicitly[Numeric[N]].times)

  /** @return a times fold from a mapping to a Num */
  def timesBy[A, N : Numeric](f: A => N): FoldState[A, N]  =
    times[N].contramap[A](f)

  /** @return a maximum fold */
  def maximum[T : Order]: FoldState[T, Option[T]] =
    fromFoldLeft(None:Option[T])((m: Option[T], t: T) => (m.toList :+ t).maximum)

  /** @return a maximum fold by a given member */
  def maximumBy[A, T : Order](f: A => T): FoldState[A, Option[A]] =
    fromFoldLeft(None:Option[A])((m: Option[A], a: A) => (m.toList :+ a).maximumBy(f))

  /** @return a maximum fold of a given member */
  def maximumOf[A, T : Order](f: A => T): FoldState[A, Option[T]] =
    maximum[T].contramap[A](f)

  /** @return a minimum fold */
  def minimum[T : Order]: FoldState[T, Option[T]] =
    fromFoldLeft(None:Option[T])((m: Option[T], t: T) => (m.toList :+ t).minimum)

  /** @return a minimum fold by a given member */
  def minimumBy[A, T : Order](f: A => T): FoldState[A, Option[A]] =
    fromFoldLeft(None:Option[A])((m: Option[A], a: A) => (m.toList :+ a).minimumBy(f))

  /** @return a minimum fold of a given member */
  def minimumOf[A, T : Order](f: A => T): FoldState[A, Option[T]] =
    minimum[T].contramap[A](f)

  /** @return the mean of elements */
  def mean[N : Fractional]: Fold[N, N] { type S = (N, Int) } =
    plus.zip(count).map { case (s, c) =>
      val frac = implicitly[Fractional[N]]; import frac._

      if (c == 0) frac.zero
      else        s / fromInt(c)
    }

  /** @return the number of elements, mean and standard deviation */
  def stddev[N : Fractional]: Fold[N, Double] =
    onlineStddev.map(_._3)

  /** @return the number of elements, mean and standard deviation */
  def onlineStddev[N : Fractional]: Fold[N, (Int, N, Double)] =
    onlineVariance map { case (count, mean, variation) =>
      implicit val num = implicitly[Fractional[N]]; import num._
      (count, mean, math.sqrt(toDouble(variation)))
    }

  /** @return the number of elements, mean and variance */
  def onlineVariance[N : Fractional]: Fold[N, (Int, N, N)] =
    onlineVariation map { case (count, mean, variation) =>
      implicit val num = implicitly[Fractional[N]]; import num._

      if (count <= 1) (count, mean, variation)
      else            (count, mean, variation / fromInt(count))
    }

  /** @return the number of elements, mean and unbiased variance */
  def onlineUnbiasedVariance[N : Fractional]: Fold[N, (Int, N, N)] =
    onlineVariation map { case (count, mean, variation) =>
      implicit val num = implicitly[Fractional[N]]; import num._

      if (count <= 1) (count, mean, variation)
      else            (count, mean, variation / fromInt(count - 1))
    }

  /** @return the number of elements, mean and variation */
  def onlineVariation[N : Fractional]: Fold[N, (Int, N, N)] = new Fold[N, (Int, N, N)] {
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
  def randomInt[T] =
    randomWithGeneratorAndFunction[T, Int](new util.Random, (_:util.Random).nextInt)

    /** a fold where the current state is a random Int */
  def randomIntWithSeed[T](seed: Long) =
    randomWithGeneratorAndFunction[T, Int](new util.Random(seed), (_:util.Random).nextInt)

  /** a fold where the current state is a random Double */
  def randomDouble[T] =
    randomWithGeneratorAndFunction[T, Double](new util.Random, (_:util.Random).nextDouble)

  /** a fold where the current state is a random Double */
  def randomDoubleWithSeed[T](seed: Long) =
    randomWithGeneratorAndFunction[T, Double](new util.Random(seed), (_:util.Random).nextDouble)

  /** create a fold for a mutable Random object */
  def randomWithGeneratorAndFunction[T, R](random: util.Random, f: util.Random => R) = new Fold[T, Option[R]] {
    type S = (util.Random, Option[R])
    def start = (random, None)
    def fold = (s: S, t: T) => { val r = f(s._1); (s._1, Some(r)) }
    def end(s: S) = Some(f(s._1))
  }

  /**
   * return an arbitrary streamed element so that each element has the same probability
   * be chosen
   */
  def reservoirSampling[T] = new Fold[T, Option[T]] {
    type S = (scala.util.Random, Int, Option[T])
    def start = (new scala.util.Random, 0, None)
    def fold = (s: S, t: T) => {
      val (random, n, selected) = s
      val newSelection =
        selected match {
          case Some(a) =>
            val r = random.nextInt(n + 1) + 1
            if (r == 1) Some(t) else selected

        case None => Some(t)
      }
      (random, n + 1, newSelection)
    }
    def end(s: S) = s._3
  }

  /** @return a Fold which simply accumulates elements into a List */
  def list[T]: Fold[T, List[T]] = new Fold[T, List[T]] {
    // a ListBuffer is used for efficient appends
    type S = scala.collection.mutable.ListBuffer[T]
    def start = new scala.collection.mutable.ListBuffer[T]
    def fold = (s: S, t: T) => { s.append(t); s }
    def end(s: S) = s.toList
  }

  /** checksums */
  // read bytes, an array of bytes + the number of bytes read
  type Bytes = (Array[Byte], Int)

  def md5: Fold[Array[Byte], String] =
    checksum("MD5")

  def sha1: Fold[Array[Byte], String] =
    checksum("SHA1")

  def md5Bytes: Fold[Bytes, String] =
    checksumBytes("MD5")

  def sha1Bytes: Fold[Bytes, String] =
    checksumBytes("SHA1")

  def checksum(algorithm: String): Fold[Array[Byte], String] =
    checksumBytes(algorithm).contramap[Array[Byte]](a => (a, a.length))

  def checksumBytes(algorithm: String): Fold[Bytes, String] =
    messageDigestBytes(algorithm).map(_.map("%02X".format(_)).mkString.toLowerCase)

  def messageDigestBytes(algorithm: String): Fold[Bytes, Array[Byte]] = new Fold[Bytes, Array[Byte]] {
    type S = MessageDigest
    def start = MessageDigest.getInstance(algorithm)
    def fold = (md, bytes) => { md.update(bytes._1, 0, bytes._2); md }
    def end(s: S) = s.digest
  }
  */
}

object Folds extends Folds
