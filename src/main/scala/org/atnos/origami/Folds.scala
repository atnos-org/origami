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
}

object Folds extends Folds
