package org.atnos
package origami
package effect

import scalaz._, Scalaz._, effect._
import scalaz.concurrent.Task
import NonEmptyList._
import collection.JavaConverters._
import SafeT._
import collection.mutable.ListBuffer
import java.util.concurrent.ConcurrentLinkedQueue

/**
 * Monad transformer for "safe" resources
 */
case class SafeT[M[_], A](private val exec: ConcurrentLinkedQueue[Finalizer[M]] => M[A]) {
  type Finalizers = ConcurrentLinkedQueue[Finalizer[M]]

  private def finalizeT(finalizers: Finalizers)(implicit m: Monad[M], c: Catchable[M]): M[Option[FinalizersException]] = {
    finalizers.asScala.toList.reverse.foldLeft(m.point(None: Option[FinalizersException])) { (res, cur) =>
      c.attempt(cur.run) >>= { a =>
        a.fold(
          l => res.map(_.map(_.add(l)).orElse(Some(FinalizersException.create(l)))),
          r => res
        )
      }
    }
  }

  def run(implicit m: Monad[M], c: Catchable[M]): M[A] =
    attemptRun(m, c).flatMap { case (res, f) =>
      res.fold(
        l => c.fail[A](l),
        r => f.map(c.fail[A]).getOrElse(m.point(r))
      )
    }

  def attemptRun(implicit m: Monad[M], c: Catchable[M]): M[(Throwable \/ A, Option[FinalizersException])] =
    runSafeT(new Finalizers)

  def runSafeT(finalizers: Finalizers)(implicit m: Monad[M], c: Catchable[M]): M[(Throwable \/ A, Option[FinalizersException])] = {
    c.attempt(exec(finalizers)) >>= { a =>
      val finalized = finalizeT(finalizers)
      finalized.map((a, _))
    }
  }

  def flatMap[B](f: A => SafeT[M, B])(implicit m: Monad[M], c: Catchable[M]): SafeT[M, B] =
    SafeT[M, B] { finalizers =>
      m.bind(exec(finalizers)) { a =>
        f(a).exec(finalizers)
      }
    }

  def `finally`(after: M[Unit])(implicit m: Monad[M], c: Catchable[M]): SafeT[M, A] =
    SafeT[M, A](finalizers => { finalizers add Finalizer(after); exec(finalizers) })
}

object SafeT extends SafeTTypes with SafeTFunctions with SafeTImplicits {

  def SafeTMonad[M[_] : Monad : Catchable]: Monad[SafeT[M, ?]] = new Monad[SafeT[M, ?]] {
    def point[A](a: =>A): SafeT[M, A] =
      SafeT[M, A](_ => Monad[M].point(a))

    def bind[A, B](st: SafeT[M, A])(f: A => SafeT[M, B]): SafeT[M, B] =
      st.flatMap(f)
  }

  def SafeTCatchable[M[_] : Monad : Catchable]: Catchable[SafeT[M, ?]] = new Catchable[SafeT[M, ?]] {
    def attempt[A](action: SafeT[M, A]): SafeT[M, Throwable \/ A] =
      SafeT[M, Throwable \/ A](finalizers => Catchable[M].attempt(action.exec(finalizers)))

    def fail[A](throwable: Throwable): SafeT[M, A] =
      SafeT[M, A](_ => Catchable[M].fail(throwable))
  }
}

trait SafeTTypes {
  // alias for IO with safe resources management
  type SafeTIO[A] = SafeT[IO, A]

  // alias for Task with safe resources management
  type SafeTTask[A] = SafeT[Task, A]
}

object SafeTTypes extends SafeTTypes

trait SafeTFunctions { outer =>
  def `finally`[M[_] : Monad : Catchable, A](action: M[A])(after: M[Unit]): SafeT[M, A] =
    bracket(Monad[M].point(()))(_ => action)(_ => after)

  def bracket[M[_] : Monad : Catchable, A, B, C](acquire: M[A])(step: A => M[B])(release: A => M[C]): SafeT[M, B] = {
    implicit val m = SafeTMonad[M]

    for {
      a <- lift(acquire)
      b <- lift(step(a)) `finally` release(a).void
    } yield b
  }

  def bracket_[M[_] : Monad : Catchable, A, B, C](before: M[A])(action: M[B])(after: M[C]): SafeT[M, B] =
    bracket(before)(_ => action)(_ => after)

  def lift[M[_] : Monad : Catchable, A](ma: M[A]): SafeT[M, A] =
    SafeT[M, A](_ => ma)

  def point[M[_] : Monad : Catchable, A](a: =>A): SafeT[M, A] =
    lift(Monad[M].point(a))
}

object SafeTFunctions extends SafeTFunctions

trait SafeTImplicits {
  implicit class Finally[M[_] : Monad : Catchable, A](action: M[A]) {
    def `finally`(after: M[Unit]): SafeT[M, A] =
      SafeTFunctions.`finally`(action)(after)
  }

  implicit def SafeTIOMonad: Monad[SafeT[IO, ?]] =
    SafeTMonad[IO]

  implicit def SafeTTaskMonad: Monad[SafeT[Task, ?]] =
    SafeTMonad[Task]

  implicit def SafeTTaskCatchable: Catchable[SafeT[Task, ?]] =
    SafeTCatchable[Task]

  implicit def SafeTIOCatchable: Catchable[SafeT[IO, ?]] =
    SafeTCatchable[IO]
}

object SafeTImplicits extends SafeTImplicits

trait Finalizer[M[_]] {
  def run: M[Unit]
}

object Finalizer {
  def apply[M[_]](r: M[Unit]) = new Finalizer[M] {
    def run = r
  }
}

case class FinalizersException(private val _errors: NonEmptyList[Throwable]) extends
  Exception(_errors.reverse.map(_.getMessage).toList.mkString(", ")) {

  def errors = _errors.reverse

  def add(t: Throwable) =
    FinalizersException(t <:: _errors)
}

object FinalizersException {
  def create(t: Throwable): FinalizersException =
    FinalizersException(NonEmptyList.nels(t))
}
