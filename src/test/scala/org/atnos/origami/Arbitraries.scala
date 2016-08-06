package org.atnos
package origami

import FoldEff._
import Folds._
import org.scalacheck.Arbitrary._
import org.scalacheck.{Arbitrary, Gen, Prop}
import Prop._
import org.atnos.eff.NoEffect
import cats._

object Arbitraries {
/*
  type F[A, B] = FoldEff[NoEffect, A, B]
  type FInt[A] = F[Int, A]

  implicit def FoldIntStringArbitrary: Arbitrary[F[Int, String]] =
    Arbitrary(FoldIntStringArbitraryWithState.arbitrary.map(t => t))

  implicit def FoldIntStringArbitraryWithState: Arbitrary[F[Int, String] { type S = Int }] = Arbitrary {
    for {
      init <- Gen.choose(0, 10)
      fd   <- Gen.oneOf((s: Int, i: Int) => s + i, (s: Int, i: Int) => s * i)
      last <- Gen.oneOf((i: Int) => i.toString, (i: Int) => (i*2).toString)
    } yield new F[Int, String] {
      type S = Int

      def start       = init
      def fold        = fd
      def end(s: Int) = last(s)

      override def toString = {
        val foldres = this.run((1 to 10).toList)
        "int fold with init "+ init + " and fold result "+foldres
      }
    }
  }

  implicit def FoldStringStringArbitraryWithState: Arbitrary[F[String, String] { type S = Int }] = Arbitrary {
    for {
      init <- arbitrary[Int]
      fd   <- Gen.oneOf((s: Int, s2: String) => s + s2.size, (s: Int, s2: String) => s * s2.size)
      last <- Gen.oneOf((s: Int) => s.toString, (s: Int) => (s*2).toString)
    } yield new F[String, String] {
      type S = Int

      def start       = init
      def fold        = fd
      def end(s: Int) = last(s)

      override def toString = "string fold"
    }
  }

  implicit def FoldIntIntArbitrary: Arbitrary[F[Int, Int]] = Arbitrary {
    for {
      init <- Gen.choose(0, 10)
      fd   <- Gen.oneOf((s: Int, i: Int) => s + i, (s: Int, i: Int) => s * i)
      last <- Gen.oneOf((i: Int) => i, (i: Int) => (i*2))
    } yield new F[Int, Int] {
      type S = Int

      def start       = init
      def fold        = fd
      def end(s: Int) = last(s)

      override def toString = {
        val foldres = this.run((1 to 10).toList)
        "int fold with init "+ init + " and fold result "+foldres
      }
    }
  }

  implicit def FoldStringIntArbitrary: Arbitrary[F[String, Int]] = Arbitrary {
    for {
      init <- Gen.choose(0, 10).map(_.toString)
      fd   <- Gen.oneOf((s: String, i: String) => s + i, (s: String, i: String) => i + s + i)
      last <- Gen.oneOf((i: String) => i.size, (i: String) => i.size * 2)
    } yield new F[String, Int] {
      type S = String

      def start     = init
      def fold      = fd
      def end(s: S) = last(s)

      override def toString = {
        val foldres = this.run((0 to 10).toList.map(_.toString))
        "string fold with init "+ init + " and fold result "+foldres
      }
    }
  }

  implicit def FoldIntStringToIntArbitrary: Arbitrary[F[Int, String => Int]] = Arbitrary {
    for {
      init <- Gen.choose(0, 10)
      fd   <- Gen.oneOf((s: Int, i: Int) => s + i, (s: Int, i: Int) => s * i)
      last <- Gen.oneOf((i: Int) => i, (i: Int) => (i*2))
    } yield new F[Int, String => Int] {
      type S = Int

      def start       = init
      def fold        = fd
      def end(s: Int) = (string: String) => string.size+last(s)

      override def toString = {
        val foldres = this.run((0 to 10).toList)
        "int fold with init "+ init + " and fold result "+foldres
      }
    }
  }

  implicit def FoldIntIntToStringArbitrary: Arbitrary[F[Int, Int => String]] = Arbitrary {
    for {
      init <- Gen.choose(0, 10)
      fd   <- Gen.oneOf((s: Int, i: Int) => s + i, (s: Int, i: Int) => s * i)
      last <- Gen.oneOf((i: Int) => i, (i: Int) => (i*2))
    } yield new F[Int, Int => String] {
      type S = Int

      def start       = init
      def fold        = fd
      def end(s: Int) = (i: Int) => (i+last(s)).toString

      override def toString = {
        val foldres = this.run((0 to 10).toList)
        "int fold with init "+ init + " and fold result "+foldres
      }
    }
  }

  implicit def IntFoldEq[U]: Eq[F[Int, U]] = new Eq[F[Int, U]] {
    def eq(a1: F[Int, U], a2: F[Int, U]): Boolean = {
      (a1.run((1 to 3).toList) == a2.run((1 to 3).toList)) &&
        (a1.run(List[Int]()) == a2.run(List[Int]()))
    }
  }

  implicit def StringFoldEq[U]: Eq[F[String, U]] = new Eq[F[String, U]] {
    def eq(a1: F[String, U], a2: F[String, U]): Boolean =
      (a1.run((1 to 10).toList.map(_.toString)) == a2.run((1 to 10).toList.map(_.toString))) &&
        (a1.run(List[String]()) == a2.run(List[String]()))
  }

  implicit def EqString: Eq[String] =
    Eq.equalA[String]

  implicit class approxEqual(d1: Double) {
    def ==~(d2: Double) = math.abs(d1 - d2) <= 0.001
  }
  */
}

