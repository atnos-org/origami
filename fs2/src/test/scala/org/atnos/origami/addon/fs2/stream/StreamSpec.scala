package org.atnos.origami.addon.fs2.stream

import cats.Monad
import org.atnos.origami.Fold
import org.specs2._
import org.specs2.matcher.ThrownExpectations
import cats.effect._
import fs2._

import scala.collection.mutable.ListBuffer

class StreamSpec extends Specification with ThrownExpectations { def is = s2"""

  A fs2 stream can be folded with an origami Fold
    folding with 1, 2, 3         $foldStream
    folding with an empty stream $foldEmpty

"""

  def foldStream = {
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

    stream(1 to 3).foldWith(sink).unsafeRunSync === 7

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

    emptyStream.foldWith(sink).unsafeRunSync === 1

    result.toList ==== List()
  }

  /**
   * HELPERS
   */

  def stream(range: Range): Stream[IO, Int] =
    Stream.apply(range.toList:_*).evalMap(i => IO(i))

  def emptyStream: Stream[IO, Int] =
    Stream.empty

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
