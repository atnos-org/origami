package org.atnos
package origami
package effect
/*
import org.scalacheck._, Gen._, Arbitrary._, Prop._
import scala.io.Codec
import scalaz._, Scalaz._
import scalaz.concurrent.Task
import scalaz.effect._
import SafeT._
import disorder._

object SafeTSpec extends Properties("SafeTSpec") {

  property("an added finalizer must always be called") =
    addedFinalizer

  property("finalizers must be called in the call order") =
    callOrder

  property("finalizers must be called even if one of them fails") =
    finalizersError

  def addedFinalizer = forAllNoShrink { value: Value[Int] =>
    val finalizer = TestFinalizer()
    val safeT = monad.point(value.value()) `finally` finalizer.run

    safeT.run.attemptRun
    val wasCalled = finalizer.called

    wasCalled :| "the finalizer was called: "+wasCalled
  }

  def callOrder = forAllNoShrink { (value: Value[Int], values1: List[Value[Int]]) =>
    val values = value :: values1
    var order = new collection.mutable.ListBuffer[String]
    def finalizer(i: Int) = Task.delay { order.append("finalizer "+i) }

    val safeT = values1.zipWithIndex.foldLeft(point(value) `finally` finalizer(0)) { case (res, (cur, i)) =>
      res >> (point(cur) `finally` finalizer(i + 1))
    }

    safeT.run.attemptRun

    order.toList ?= ((values.takeWhile(_.isDefined) ++ values.dropWhile(_.isDefined).take(1))).zipWithIndex.map (_._2) map ("finalizer "+_)
  }

  def finalizersError = forAllNoShrink { (value: (ValueOk[Int], Value[Int]), values1: List[(ValueOk[Int], Value[Int])]) =>
   val values = value :: values1

    var order = new collection.mutable.ListBuffer[String]
    def finalizer(v: Value[Int], i: Int) = Task.delay { order.append("calling finalizer "+i); v.value(); () }

    val safeT = values1.zipWithIndex.foldLeft(value._1.run `finally` value._2.run.void) { case (res, ((cur, f), i)) =>
      res >> (cur.run `finally` finalizer(f, i + 1))
    }

    val (result, errors) = safeT.attemptRun.run
    val failedFinalizers = values.count(_._2.throwsException)

    errors.map(_.errors.size).getOrElse(0) ?= failedFinalizers
  }


  /**
   * HELPERS
   */

  def monad = SafeTMonad[Task]

  def point[A](value: Value[A]): SafeT[Task, A] =
    monad.point(value.value())

  /** class of values which can throw exceptions */
  case class Value[A](value: () => A, originalValue: A, throwsException: Boolean) {
    def run: Task[A] =
      Task.delay(value())

    def isDefined: Boolean =
      !throwsException

    override def toString = "value: "+ originalValue + (if (throwsException) " - throws Exception" else "")
  }

  object Value {
    def value[A](a: A): Value[A] =
      create(a, throwsException = false)

    def ko[A](a: A): Value[A] =
      create(a, throwsException = true)

     def create[A](a: A, throwsException: Boolean): Value[A] =
      Value(() => if (throwsException) { throw new java.lang.Exception("no value!"); a } else a, a, throwsException)
  }

  implicit def ArbitraryValue[A : Arbitrary]: Arbitrary[Value[A]] =
    Arbitrary {
      for {
        b <- arbitrary[Boolean]
        a <- arbitrary[A]
      } yield Value.create(a, b)
    }

  /** class of values which can never throw exceptions */
  case class ValueOk[A](value: A) {
    def run: Task[A] =
      Task.delay(value)

    override def toString = "value: "+ value
  }

  implicit def ArbitraryValueOk[A : Arbitrary]: Arbitrary[ValueOk[A]] =
    Arbitrary(arbitrary[A].map(ValueOk(_)))

}

case class TestFinalizer(var called: Boolean = false) extends Finalizer[Task] {
  def run: Task[Unit] = Task.delay { called = true; () }
}
*/
