package org.atnos
package origami

import java.security.MessageDigest
import org.atnos.eff.{Fold => _, _}
import eff._
import cats.{Apply, Foldable, Monoid}
import cats.implicits._
import foldEff._

/**
 * A FoldM is a "left fold" over a data structure with:
 *  - a 'start' value
 *  - a 'fold' method to accumulate state
 *  - an 'end' method to finalize the result
 *
 * Both 'start' and 'end' have an effect which allows the whole folding to take place inside a context M.
 *
 * If 'M' has an 'Apply' instance then FoldM can be made Applicative to allow the folding of two values U and S at
 * the same time.
 *
 * If 'M' has a 'Monad' instance then FoldM can be made into a 'Compose' instance which allows to
 * compose 2 folds into one, for example:
 *
 *  - 'sum' computes the sum of some elements
 *  - 'list' accumulates all the elements in a list
 *  - the 'sum compose list' will accumulate the list of all the sums over some elements (this is a 'scan')
 *
 * A FoldM can be used with a 'FoldableM' which produces the elements to fold over. Examples of FoldableM include
 *
 *  - a List
 *  - an Iterator
 *  - a scalaz Process
 *
 * Usage example:
 *
 *  sum.run(List(1, 2, 3)) == 6
 */
trait FoldEff[R, T, U] { self =>
  type S

  def start: Eff[R, S]
  def fold: (S, T) => S
  def end(s: S): Eff[R, U]

  /** map the output value */
  def map[V](f: U => V) = new FoldEff[R, T, V] {
    type S = self.S
    def start = self.start
    def fold = self.fold
    def end(s: S) = self.end(s).map(f)
  }

  /** flatMap the output value */
  def mapFlatten[V](f: U => Eff[R, V]) = new FoldEff[R, T, V] {
    type S = self.S
    def start = self.start
    def fold = self.fold
    def end(s: S) = self.end(s).flatMap(f)
  }

  /** run another fold on the end result */
  def pipe[V](f: FoldEff[R, U, V]) = new FoldEff[R, T, V] {
    type S = self.S
    def start = self.start
    def fold = self.fold
    def end(s: S) = self.end(s).flatMap(f.run1)
  }

  /** parallel composition */
  def ***[V, W](f: FoldEff[R, V, W]) = new FoldEff[R, (T, V), (U, W)] {
    type S = (self.S, f.S)
    def start = (self.start |@| f.start).map((_,_))
    def fold = (s: S, tv: (T, V)) => (self.fold(s._1, tv._1), f.fold(s._2, tv._2))
    def end(s: S) = (self.end(s._1) |@| f.end(s._2)).map((_,_))
  }

  /** fanout = zip in the Arrow terminology */
  def &&&[V](f: FoldEff[R, T, V]) =
    zip(f)

  /** contramap the input values */
  def contramap[V](f: V => T) = new FoldEff[R, V, U] {
    type S = self.S
    def start = self.start
    def fold = (s: S, v: V) => self.fold(s, f(v))
    def end(s: S) = self.end(s)
  }

  /** zip 2 folds to return a pair of values. alias for zip */
  def <*>[V](f: FoldEff[R, T, V]) =
    zip(f)

  /** zip 2 folds to return a pair of values. alias for <*> */
  def zip[V](f: FoldEff[R, T, V]) = new FoldEff[R, T, (U, V)] {
    type S = (self.S, f.S)
    def start = Apply[Eff[R, ?]].tuple2(self.start, f.start)
    def fold = (s, t) => (self.fold(s._1, t), f.fold(s._2, t))
    def end(s: S) = Apply[Eff[R, ?]].tuple2(self.end(s._1), f.end(s._2))
  }

  /** zip with another fold only for its side effects */
  def <*(f: SinkEff[R, T]) =
    zip(f).map(_._1)

  /** alias for <* */
  def observe(f: SinkEff[R, T]) =
    zip(f).map(_._1)

  /** observe both the input value and the current state */
  def observeWithState(sink: SinkEff[R, (S, T)]) = new FoldEff[R, T, U] {
    type S = (self.S, sink.S)
    def start = Apply[Eff[R, ?]].tuple2(self.start , sink.start)
    def fold = (s: S, t: T) => (self.fold(s._1, t), sink.fold(s._2, (s._1, t)))
    def end(s: S) = Apply[Eff[R, ?]].tuple2(self.end(s._1), sink.end(s._2)).map(_._1)
  }

  /** alias for observeWithState */
  def <<-*(sink: SinkEff[R, (S, T)]) =
    observeWithState(sink)

  /** observe the current state */
  def observeState(sink: SinkEff[R, S]) = new FoldEff[R, T, U] {
    type S = (self.S, sink.S)
    def start = Apply[Eff[R, ?]].tuple2(self.start , sink.start)
    def fold = (s: S, t: T) => (self.fold(s._1, t), sink.fold(s._2, s._1))
    def end(s: S) = Apply[Eff[R, ?]].tuple2(self.end(s._1), sink.end(s._2)).map(_._1)
  }

  /** alias for observeState */
  def <-*(sink: SinkEff[R, S]) =
    observeState(sink)

  /** observe both the input value and the next state */
  def observeWithNextState(sink: SinkEff[R, (S, T)]) = new FoldEff[R, T, U] {
    type S = (self.S, sink.S)
    def start = Apply[Eff[R, ?]].tuple2(self.start , sink.start)
    def fold = (s: S, t: T) => { val next = self.fold(s._1, t); (next, sink.fold(s._2, (next, t))) }
    def end(s: S) = Apply[Eff[R, ?]].tuple2(self.end(s._1), sink.end(s._2)).map(_._1)
  }

  /** alias for observeWithNextState */
  def <<+*(sink: SinkEff[R, (S, T)]) =
    observeWithNextState(sink)

  /** observe the next state */
  def observeNextState(sink: SinkEff[R, S]) = new FoldEff[R, T, U] {
    type S = (self.S, sink.S)
    def start = Apply[Eff[R, ?]].tuple2(self.start , sink.start)
    def fold = (s: S, t: T) => { val next = self.fold(s._1, t); (next, sink.fold(s._2, next)) }
    def end(s: S) = Apply[Eff[R, ?]].tuple2(self.end(s._1), sink.end(s._2)).map(_._1)
  }

  /** alias for observeNextState */
  def <+*(sink: SinkEff[R, S]) =
    observeNextState(sink)

  /**
   * run a FoldEff with a Foldable instance
   */
  def run[F[_] : Foldable](foldable: F[T]): Eff[R, U] =
    start.flatMap { s =>
      end(foldable.foldLeft(s)((res, cur) => fold(res, cur)))
    }

  /**
   * run over one element
   */
  def run1(t: T): Eff[R, U] =
    start.flatMap(s => end(fold(s, t)))

  /*


    /**
     * collect all the successive states in a list
     */
    def writeState(implicit f: Functor[M]): FoldEff[R, T, (U, List[S])] = new FoldEff[R, T, (U, List[S])] {
      type S = (self.S, scala.collection.mutable.ListBuffer[self.S])
      def start = f.map(self.start)(s => (s, new scala.collection.mutable.ListBuffer[self.S]))
      def fold = (s: S, t: T) => (self.fold(s._1, t), { s._2.append(s._1); s._2 })
      def end(s: S) = f.map(self.end(s._1))(u => (u, s._2.toList))
    }

    /**
     * collect all the successive "next" states in a list
     */
    def writeNextState(implicit f: Functor[M]): FoldEff[R, T, (U, List[S])] = new FoldEff[R, T, (U, List[S])] {
      type S = (self.S, scala.collection.mutable.ListBuffer[self.S])
      def start = f.map(self.start)(s => (s, { val l = new scala.collection.mutable.ListBuffer[self.S]; l.append(s); l }))
      def fold = (s: S, t: T) => { val newS = self.fold(s._1, t); (newS, { s._2.append(newS); s._2 }) }
      def end(s: S) = f.map(self.end(s._1))(u => (u, s._2.toList))
    }

    /** pipe the output of this fold into another fold */
    def compose[V](f2: FoldEff[R, U, V])(implicit m: Monad[M]) = new FoldEff[R, T, V] {
      type S = Eff[R, (self.S, f2.S)]
      def start = Monad[M].point(Apply[M].tuple2(self.start, f2.start))

      def fold = (s, t) =>
        s.flatMap { case (f1s, f2s) =>
          self.end(self.fold(f1s, t)).map((u: U) => (self.fold(f1s, t), f2.fold(f2s, u)))
        }

      def end(s: S) = s.flatMap { case (f1s, f2s) =>
        f2.end(f2s)
      }
    }

    /** create a fold that will run this fold repeatedly on input elements and collect all results */
    def nest[F[_], R](f: R => F[T])(implicit df: FoldableEff[R, M, F[T], T], monoid: Monoid[U], monad: Monad[M]) = new FoldEff[R, R, U] {
      type S = Eff[R, U]
      def start = monad.point(monad.point(monoid.zero))
      def fold = (s: S, r: R) =>
          Monad[M].apply2(self.run(f(r)): Eff[R, U], s: Eff[R, U])((s1: U, s2: U) => monoid.append(s1, s2))

      def end(s: S) = s
    }

    /**
     * create a Fold which will indicate that the iteration can be stopped when
     * a condition is reached
     */
    def breakWhen(predicate: S => Boolean)(implicit f: Functor[M]): FoldEff[R, T, U] { type S = self.S \/ self.S } = new  FoldEff[R, T, U] {
      type S = self.S \/ self.S

      def start =
        f.map(self.start) { s =>
          if (predicate(s)) \/-(s)
          else              -\/(s)
        }

      def fold = (s: S, t: T) => {
        s match {
          case -\/(s1) =>
            val newState = self.fold(s1, t)
            if (predicate(newState)) \/-(newState)
            else                     -\/(newState)

          case \/-(s1) => \/-(self.fold(s1, t))
        }
      }

      def end(s: S) = s.fold(self.end _, self.end _)
    }

    /**
     * run a FoldM with a FoldableM instance (like a List, an Iterator, a scalaz Process)
     * and break early if possible
     */
    def runBreak[F, V](ft: F)(implicit foldableM: FoldableEff[R, M, F, T], ev: S <:< (V \/ V)): Eff[R, U] =
      foldableM.foldMBreak(ft)(self.asInstanceOf[FoldEff[R, T, U] { type S = V \/ V }])
  */

/*
  /**
   * use a natural transformation to go from context M to context N
   * this can be used to transform a FoldEff[R, A, Id, B] into a FoldEff[R, A, Task, B] for example
   * (a fold with no effects to a fold with monadic effects from the Task monad)
   */
  def into[N[_]](implicit nat: M ~> N) = new FoldEff[R, N, T, U] {
    type S = self.S
    def start = nat(self.start)
    def fold = (s, t) => self.fold(s, t)
    def end(s: S) = nat(self.end(s))
  }

  /** equivalent of the as method for functors, added here for easier type inference */
  def as[V](v: =>V)(implicit m: Functor[M]) =
    map(_ => v)

  /** equivalent of the void method for functors, added here for easier type inference */
  def void(implicit m: Functor[M]) =
    as(())

    */
}

/**
 * Typeclass instances and creation methods for folds
 */
object FoldEff extends FoldEffTypes with FoldEffFunctions

trait FoldEffTypes {
  /** alias for a non-effectful Fold */
  type Fold[T, U] = FoldEff[NoFx, T, U]

  /** alias for a non-effectful Fold where the state type is U */
  type FoldState[T, U] = FoldEff[NoFx, T, U] { type S = U }

  /** alias for a Fold sinking its last value */
  type SinkEff[R, T] = FoldEff[R, T, Unit]
}

object FoldEffTypes extends FoldEffTypes

trait FoldEffFunctions {

  /** @return a fold which uses a Monoid to accumulate elements */
  def fromMonoidMap[T, M : Monoid](f: T => M) = new Fold[T, M] {
    type S = M
    def start = pure(Monoid[M].empty)
    def fold = (s: S, t: T) => Monoid[M].combine(s, f(t))
    def end(s: S) = pure(s)
  }

  /** @return a fold from arguments of a fold left */
  def fromFoldLeft[R, T, U](u: U)(f: (U, T) => Eff[R, U]) = new FoldEff[R, T, U] {
    type S = Eff[R, U]
    def start = pure(pure(u))
    def fold = (s: S, t: T) => s.flatMap(u => f(u, t))
    def end(s: S) = s
  }

}
