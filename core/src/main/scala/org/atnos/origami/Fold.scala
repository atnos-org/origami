package org.atnos
package origami

import cats._
import cats.free._
import cats.data.State
import cats.arrow.{Category, Compose}
import cats.functor.Profunctor
import cats.syntax.flatMap._
import cats.syntax.apply._
import cats.syntax.cartesian._
import cats.syntax.foldable._
import cats.syntax.functor._
import FoldCreation._

/**
 * A Fold is a "left fold" over a data structure with:
 *  - a 'start' value
 *  - a 'fold' method to accumulate state
 *  - an 'end' method to finalize the result
 *
 * Those 3 methods use a monad for effects
 *
 */
trait Fold[M[_], A, B] { self =>
  implicit def monad: Monad[M]

  type S

  def start: M[S]
  def fold: (S, A) => M[S]
  def end(s: S): M[B]

  /** map the output value */
  def map[C](f: B => C) = new Fold[M, A, C] {
    type S = self.S
    implicit val monad: Monad[M] = self.monad

    def start = self.start
    def fold = self.fold
    def end(s: S) = self.end(s).map(f)
  }

  /** flatMap the output value */
  def mapFlatten[C](f: B => M[C]) = new Fold[M, A, C] {
    type S = self.S
    implicit val monad: Monad[M] = self.monad

    def start = self.start
    def fold = self.fold
    def end(s: S) = self.end(s).flatMap(f)
  }

  /** run another fold on the end result */
  def pipe[C](f: Fold[M, B, C]) = new Fold[M, A, C] {
    type S = self.S
    implicit val monad: Monad[M] = self.monad

    def start = self.start
    def fold = self.fold
    def end(s: S) = self.end(s).flatMap(f.run1)
  }

  /** parallel composition */
  def ***[V, W](f: Fold[M, V, W]) = new Fold[M, (A, V), (B, W)] {
    type S = (self.S, f.S)
    implicit val monad: Monad[M] = self.monad

    def start = monad.tuple2(self.start, f.start)
    def fold = (s: S, av: (A, V)) => monad.tuple2(self.fold(s._1, av._1), f.fold(s._2, av._2))
    def end(s: S) = monad.tuple2(self.end(s._1), f.end(s._2))
  }

  /** fanout = zip in the Arrow terminology */
  def &&&[C](f: Fold[M, A, C]) =
    zip(f)

  /** contramap the input values */
  def contramap[C](f: C => A) = new Fold[M, C, B] {
    type S = self.S
    implicit val monad: Monad[M] = self.monad

    def start = self.start
    def fold = (s: S, c: C) => self.fold(s, f(c))
    def end(s: S) = self.end(s)
  }

  /** zip 2 folds to return a pair of values. alias for zip */
  def <*>[C](f: Fold[M, A, C]) =
    zip(f)

  /** zip 2 folds to return a pair of values. alias for <*> */
  def zip[C](f: Fold[M, A, C]) = new Fold[M, A, (B, C)] {
    type S = (self.S, f.S)
    implicit val monad: Monad[M] = self.monad

    def start = monad.tuple2(self.start, f.start)
    def fold = (s, a) => monad.tuple2(self.fold(s._1, a), f.fold(s._2, a))
    def end(s: S) = monad.tuple2(self.end(s._1), f.end(s._2))
  }

  /** zip with another fold, running this one only for its side effects */
  def *>[C](f: Fold[M, A, C]): Fold[M, A, C] =
    zip(f).map(_._2)

  /** alias for *> */
  def observedBy[C](f: Fold[M, A, C]): Fold[M, A, C] =
    zip(f).map(_._2)

  /** zip with another fold only for its side effects */
  def <*[C](f: Fold[M, A, C]) =
    zip(f).map(_._1)

  /** alias for <* */
  def observe[C](f: Fold[M, A, C]) =
    zip(f).map(_._1)

  /** observe both the input value and the current state */
  def observeWithState(sink: Sink[M, (A, S)]) = new Fold[M, A, B] {
    type S = (self.S, sink.S)
    implicit val monad: Monad[M] = self.monad

    def start = monad.tuple2(self.start , sink.start)
    def fold = (s: S, a: A) => monad.tuple2(self.fold(s._1, a), sink.fold(s._2, (a, s._1)))
    def end(s: S) = monad.tuple2(self.end(s._1), sink.end(s._2)).map(_._1)
  }

  /** alias for observeWithState */
  def <<-*(sink: Sink[M, (A, S)]) =
    observeWithState(sink)

  /** observe the current state */
  def observeState(sink: Sink[M, S]) = new Fold[M, A, B] {
    type S = (self.S, sink.S)
    implicit val monad: Monad[M] = self.monad

    def start = monad.tuple2(self.start , sink.start)
    def fold = (s: S, a: A) => monad.tuple2(self.fold(s._1, a), sink.fold(s._2, s._1))
    def end(s: S) = monad.tuple2(self.end(s._1), sink.end(s._2)).map(_._1)
  }

  /** alias for observeState */
  def <-*(sink: Sink[M, S]) =
    observeState(sink)

  /** observe both the input value and the next state */
  def observeWithNextState(sink: Sink[M, (A, S)]) = new Fold[M, A, B] {
    type S = (self.S, sink.S)
    implicit val monad: Monad[M] = self.monad

    def start = monad.tuple2(self.start , sink.start)
    def fold = (s: S, a: A) => self.fold(s._1, a).flatMap(next => sink.fold(s._2, (a, next)).map((next, _)))
    def end(s: S) = monad.tuple2(self.end(s._1), sink.end(s._2)).map(_._1)
  }

  /** alias for observeWithNextState */
  def <<+*(sink: Sink[M, (A, S)]) =
    observeWithNextState(sink)

  /** observe the next state */
  def observeNextState(sink: Sink[M, S]) = new Fold[M, A, B] {
    type S = (self.S, sink.S)
    implicit val monad: Monad[M] = self.monad

    def start = monad.tuple2(self.start , sink.start)
    def fold = (s: S, a: A) => self.fold(s._1, a).flatMap(next => sink.fold(s._2, next).map((next, _)))
    def end(s: S) = monad.tuple2(self.end(s._1), sink.end(s._2)).map(_._1)
  }

  /** alias for observeNextState */
  def <+*(sink: Sink[M, S]) =
    observeNextState(sink)

  /**
   * run a Fold with a Foldable instance
   */
  def run[F[_] : Foldable](foldable: F[A]): M[B] =
    start.flatMap(s => Free.foldLeftM(foldable, s)(fold).flatMap(end))
  
  /**
   * run over one element
   */
  def run1(a: A): M[B] =
    start.flatMap(s => fold(s, a).flatMap(end))


  /** pipe the output of this fold into another fold */
  def compose[C](f2: Fold[M, B, C]) = new Fold[M, A, C] {
    type S = (self.S, f2.S)
    implicit val monad: Monad[M] = self.monad

    def start = monad.tuple2(self.start, f2.start)

    def fold = (s, a) =>
     self.fold(s._1, a).flatMap(self.end).flatMap((u: B) => monad.tuple2(self.fold(s._1, a), f2.fold(s._2, u)))

    def end(s: S) = 
      f2.end(s._2)
  }

  /** create a fold that will run this fold repeatedly on input elements and collect all results */
  def nest[F[_], C](f: C => F[A])(implicit monoid: Monoid[B], foldable: Foldable[F]) = new Fold[M, C, B] {
    type S = B
    implicit val monad: Monad[M] = self.monad

    def start = monad.pure(monoid.empty)

    def fold = (s: S, c: C) =>
      self.run(f(c)).map((b: B) => monoid.combine(s, b))

    def end(s: S) = monad.pure(s)
  }

  /** equivalent of the as method for functors, added here for easier type inference */
  def as[C](c: =>C) =
    map(_ => c)

  /** equivalent of the void method for functors, added here for easier type inference */
  def void =
    as(())

  def startWith(action: M[Unit]): Fold[M, A, B] = new Fold[M, A, B] {
    type S = self.S
    implicit val monad: Monad[M] = self.monad

    def start = action >> self.start
    def fold = (s, a) => self.fold(s, a)
    def end(s: S) = self.end(s)
  }

  def endWith(action: M[Unit]): Fold[M, A, B] = new Fold[M, A, B] {
    type S = self.S
    implicit val monad: Monad[M] = self.monad

    def start = self.start
    def fold = (s, a) => self.fold(s, a)
    def end(s: S) = self.end(s).flatMap(b => action.as(b))
  }

  def into[M1[_]](implicit ev: M[_] <:< Id[_], m: Monad[M1]) =
    monadic[M1](s => Monad[M1].pure(s))(ev, m)

  def monadic[M1[_]](f: S => M1[S])(implicit ev: M[_] <:< Id[_], m: Monad[M1]) = new Fold[M1, A, B] {
    type S = self.S
    implicit val monad: Monad[M1] = m

    def start = f(self.start.asInstanceOf[self.S])
    def fold = (s, a) => f(self.fold(s, a).asInstanceOf[S])
    def end(s: S) = monad.pure(self.end(s).asInstanceOf[B])
  }

}

object Fold {

  implicit def MonoidSink[M[_], A](implicit m: Monad[M]): Monoid[Fold[M, A, Unit]]= new Monoid[Fold[M, A, Unit]] {
    def empty = FoldCreation.fromStart(m.pure(()))

    def combine(s1: Fold[M, A, Unit], s2: Fold[M, A, Unit]): Fold[M, A, Unit] = new Fold[M, A, Unit] {
      implicit val monad: Monad[M] = m

      type S = (s1.S, s2.S)
      def start = monad.tuple2(s1.start, s2.start)
      def fold = (s: S, a: A) => monad.tuple2(s1.fold(s._1, a), s2.fold(s._2, a))
      def end(s: S) = monad.followedBy(s1.end(s._1))(s2.end(s._2))
    }
  }

  /**
   * Apply instance
   *
   * This means that we can write:
   *
   *   val mean: Fold[Int, Int] = (sum |@| count)(_ / _)
   *
   * An Apply instance is also a Functor instance so we can write:
   *
   *   val meanTimes2 = mean.map(_ * 2)
   */
  implicit def ApplyFold[M[_] : Monad, T]: Apply[Fold[M, T, ?]] = new Apply[Fold[M, T, ?]] {
    type F[U] = Fold[M, T, U]

    def map[A, B](fa: F[A])(f: A => B): F[B] =
      fa map f

    def ap[A, B](f: F[A => B])(fa: F[A]): F[B] =
      map(fa zip f) { case (a, b) => b(a) }
  }

  /**
   *  Profunctor instance
   *
   *  This is especially useful because we can "map" on the input element
   *
   *  val doubleSum = fromMonoid[Double] // sum all elements
   *  val roundedDoubleSum = doubleSum.mapfst(_.round)
   */
  implicit def ProfunctorFold[M[_] : Monad]: Profunctor[Fold[M, ?, ?]] = new Profunctor[Fold[M, ?, ?]] {
    type =>:[A, B] = Fold[M, A, B]

    def dimap[A, B, C, D](fab: A =>: B)(f: C => A)(g: B => D): C =>: D =
      fab.contramap(f).map(g)

  }

  /**
   * A Fold can be turned into a Compose
   *
   * This allows us to write:
   *
   * val scans = sum compose list
   *
   */
  implicit def ComposeFold[M[_] : Monad]: Compose[Fold[M, ?, ?]] = new Compose[Fold[M, ?, ?]] {
    type F[A, B] = Fold[M, A, B]

    def compose[A, B, C](f: F[B, C], g: F[A, B]): F[A, C] =
      g compose f
  }

  /**
   * Cobind instance
   */
  def CoflatMapFold[M[_], T](implicit m: Monad[M]): CoflatMap[Fold[M, T, ?]] = new CoflatMap[Fold[M, T, ?]] {
    type F[U] = Fold[M, T, U]

    def coflatMap[A, B](fa: F[A])(f: F[A] => B): F[B] = new Fold[M, T, B] {
      type S = fa.S
      implicit val monad: Monad[M] = m

      def start = fa.start
      def fold = fa.fold
      def end(s: S) = monad.pure(f(fa))
    }

    def map[A, B](fa: F[A])(f: A => B): F[B] =
      fa map f
  }
}

/** alias for a non-effectful Fold */
trait FoldId[A, U] extends Fold[Id, A, U] {
  val monad: Monad[Id] = catsInstancesForId
}

/**
 * Creation methods for folds
 */
trait FoldCreation {

  /** @return a fold which uses a Monoid to accumulate elements */
  def fromMonoidMap[A, O : Monoid](f: A => O) = new FoldId[A, O] {
    type S = O

    def start = Monoid[O].empty
    def fold = (s: S, a: A) => Monoid[O].combine(s, f(a))
    def end(s: S) = s
  }

  /** @return a fold which uses a Monoid to accumulate elements */
  def fromMonoid[A : Monoid] =
    fromMonoidMap[A, A](identity)

  /** @return a fold from arguments of a fold left */
  def fromFoldLeft[A, B](b: B)(f: (B, A) => B) = new FoldId[A, B] {
    type S = B

    def start = b
    def fold = (s: S, a: A) => f(s, a)
    def end(s: S) = s
  }

  /** @return a fold from running a State object */
  def fromStateRun[A, B, C](state: A => State[B, C])(init: B) = new FoldId[A, (B, Option[C])] {
    type S = (B, Option[C])

    def start = (init, None)
    def fold = (s: S, a: A) => {
      val (sa, c) = s
      val (newState, newC) = state(a).run(sa).value
      (newState, Some(newC))
    }
    def end(s: S) = s
  }

  /** @return a fold for the execution of a State object */
  def fromStateExec[A, B, C](state: A => State[B, C])(init: B) =
    fromStateRun(state)(init).map(_._1)

  /** @return a fold for the evaluation of a State object */
  def fromStateEval[A, B, C](state: A => State[B, C])(init: B) =
    fromStateRun(state)(init).map(_._2)

  /** @return a fold with just a start action */
  def fromStart[M[_], A, S1](action: M[S1])(implicit m: Monad[M]) = new Fold[M, A, S1] {
    type S = S1
    implicit val monad = m

    def start = action
    def fold = (s: S, a: A) => monad.pure(s)
    def end(s: S) = monad.pure(s)
  }

  /**
   * @return a fold with just an open action and a close action which is always executed even in case
   *         of a failure during folding
   */
  def bracket[M[_], A, C](open: M[C])(step: (C, A) => M[C])(close: C => M[Unit])(implicit m: MonadError[M, Throwable]): Fold[M, A, Unit] = new Fold[M, A, Unit] {
    type S = C
    implicit val monad: Monad[M] = m

    def start = open
    def fold = (s: S, a: A) => m.handleErrorWith(step(s, a))(_ => close(s).as(s))
    def end(s: S) = close(s)
  }

  /** @return a fold sinking all the values to a unit action */
  def fromSink[M[_], A](action: A => M[Unit])(implicit m: Monad[M]): Fold[M, A, Unit] = new Fold[M, A, Unit] {
    type S = Unit
    implicit val monad: Monad[M] = m

    def start = monad.pure(())
    def fold = (s: S, a: A) => action(a)
    def end(s: S) = monad.pure(())
  }
}

object FoldCreation extends FoldCreation
