package org.atnos.origami.addon.scalaz.stream

import org.specs2.Specification

import scalaz.stream._
import scalaz.effect.IO
import cats.Monad
import org.atnos.origami.Fold
import org.specs2.matcher.ThrownExpectations
import scala.collection.mutable.ListBuffer

class ProcessSpec extends Specification with ThrownExpectations { def is = s2"""

  A scalaz-stream process can be folded with an origami Fold
    folding with 1, 2, 3          $foldProcess
    folding with an empty process $foldEmpty

"""

  def foldProcess = {
    val result = new ListBuffer[Int]

    val sink = new Fold[IO, Int, Int] {
      val monad = IOMonad
      type S = Int
      def start = IO(0)
      def fold = (s: S, a: Int) => IO {
        result.append(a)
        s + a
      }
      def end(s: S) = IO(s + 1)
    }

    process(1 to 3).foldWith(sink).unsafePerformIO === 7
    result.toList ==== List(1, 2, 3)
  }

  def foldEmpty = {
    val result = new ListBuffer[Int]

    val sink = new Fold[IO, Int, Int] {
      val monad = IOMonad
      type S = Int
      def start = IO(0)
      def fold = (s: S, a: Int) => IO {
        result.append(a)
        s + a
      }
      def end(s: S) = IO(s + 1)
    }

    emptyProcess.foldWith(sink).unsafePerformIO === 1
    result.toList ==== List()
  }

  /**
   * HELPERS
   */

  def process(range: Range): Process[IO, Int] =
    Process(range.toList:_*).evalMap(i => IO(i))

  def emptyProcess: Process[IO, Int] =
    Process().evalMap(i => IO(i))

  val IOMonad = new Monad[IO] {
    def pure[A](a: A) = IO(a)
    def flatMap[A, B](fa: IO[A])(f: A => IO[B]) = fa.flatMap(f)
    def tailRecM[A, B](a: A)(f: A => IO[Either[A, B]]): IO[B] =
      f(a).flatMap {
        case Left(a1) => tailRecM(a1)(f)
        case Right(b) => IO(b)
      }
  }
}
