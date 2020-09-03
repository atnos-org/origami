package org.atnos.origami

import org.scalacheck.Arbitrary._
import org.scalacheck.{Arbitrary, Gen}
import cats._
import cats.data._

object Arbitraries {

  type FoldInt[A] = FoldId[Int, A]
  type FoldString[A] = FoldId[String, A]

  implicit def FoldIntStringArbitrary: Arbitrary[FoldInt[String]] =
    Arbitrary(FoldIntStringArbitraryWithState.arbitrary.map(t => t))

  implicit def FoldIntStringArbitraryWithState: Arbitrary[FoldInt[String] { type S = Int }] = Arbitrary {
    for {
      init <- Gen.choose(0, 10)
      fd   <- Gen.oneOf((s: Int, i: Int) => s + i, (s: Int, i: Int) => s * i)
      last <- Gen.oneOf((i: Int) => i.toString, (i: Int) => (i*2).toString)
    } yield new FoldId[Int, String] {
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

  implicit def FoldStringStringArbitraryWithState: Arbitrary[FoldString[String] { type S = Int }] = Arbitrary {
    for {
      init <- arbitrary[Int]
      fd   <- Gen.oneOf((s: Int, s2: String) => s + s2.size, (s: Int, s2: String) => s * s2.size)
      last <- Gen.oneOf((s: Int) => s.toString, (s: Int) => (s*2).toString)
    } yield new FoldId[String, String] {
      type S = Int

      def start       = init
      def fold        = fd
      def end(s: Int) = last(s)

      override def toString = "bytes fold"
    }
  }

  implicit def FoldIntIntArbitrary: Arbitrary[FoldInt[Int]] = Arbitrary {
    for {
      init <- Gen.choose(0, 10)
      fd   <- Gen.oneOf((s: Int, i: Int) => s + i, (s: Int, i: Int) => s * i)
      last <- Gen.oneOf((i: Int) => i, (i: Int) => (i*2))
    } yield new FoldInt[Int] {
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

  implicit def FoldStringIntArbitrary: Arbitrary[FoldString[Int]] = Arbitrary {
    for {
      init <- Gen.choose(0, 10).map(_.toString)
      fd   <- Gen.oneOf((s: String, i: String) => s + i, (s: String, i: String) => i + s + i)
      last <- Gen.oneOf((i: String) => i.size, (i: String) => i.size * 2)
    } yield new FoldId[String, Int] {
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

  implicit def FoldIntStringToIntArbitrary: Arbitrary[FoldInt[String => Int]] = Arbitrary {
    for {
      init <- Gen.choose(0, 10)
      fd   <- Gen.oneOf((s: Int, i: Int) => s + i, (s: Int, i: Int) => s * i)
      last <- Gen.oneOf((i: Int) => i, (i: Int) => i*2)
    } yield new FoldId[Int, String => Int] {
      type S = Int

      def start       = init
      def fold        = fd
      def end(s: Int) = (string: String) => string.size + last(s)

      override def toString = {
        val foldres = this.run((0 to 10).toList)
        "int fold with init "+ init + " and fold result "+foldres
      }
    }
  }

  implicit def FoldIntIntToStringArbitrary: Arbitrary[FoldInt[Int => String]] = Arbitrary {
    for {
      init <- Gen.choose(0, 10)
      fd   <- Gen.oneOf((s: Int, i: Int) => s + i, (s: Int, i: Int) => s * i)
      last <- Gen.oneOf((i: Int) => i, (i: Int) => i*2)
    } yield new FoldId[Int, Int => String] {
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

  implicit def NonEmptyListArbitrary[T : Arbitrary]: Arbitrary[NonEmptyList[T]] =
    Arbitrary {
      for {
        t  <- arbitrary[T]
        ls <- arbitrary[List[T]]
      } yield NonEmptyList(t, ls)
    }

  implicit def IntFoldEqual[U]: Eq[FoldInt[U]] = new Eq[FoldInt[U]] {
    def eqv(a1: FoldInt[U], a2: FoldInt[U]): Boolean = {
      (a1.run((1 to 3).toList) == a2.run((1 to 3).toList)) &&
        (a1.run(List[Int]()) == a2.run(List[Int]()))
    }
  }

  implicit def StringFoldEqual[U]: Eq[FoldString[U]] = new Eq[FoldString[U]] {
    def eqv(a1: FoldString[U], a2: FoldString[U]): Boolean =
      (a1.run((1 to 10).toList.map(_.toString)) == a2.run((1 to 10).toList.map(_.toString))) &&
        (a1.run(List[String]()) == a2.run(List[String]()))
  }

  implicit def EqualString: Eq[String] =
    Eq.fromUniversalEquals[String]

  implicit class approxEqual(d1: Double) {
    def ==~(d2: Double) = math.abs(d1 - d2) <= 0.001
  }

}
