package org.atnos
package origami

import java.io.File
import java.util.UUID

import FoldM._
import Folds$._
import FoldableM._
import org.scalacheck.Arbitrary._
import org.scalacheck.{Prop, Gen, Arbitrary}, Prop._

import scalaz.Id._
import scalaz.effect.IO
import scalaz.std.list._
import scalaz.{Equal, NonEmptyList}

object Arbitraries {

  type F[A, B] = FoldM[Id, A, B]
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

  implicit def FoldBytesStringArbitraryWithState: Arbitrary[F[Bytes, String] { type S = Int }] = Arbitrary {
    for {
      init <- arbitrary[String]
      fd   <- Gen.oneOf((s: Int, bs: Bytes) => s + bs._2, (s: Int, bs: Bytes) => s * bs._2)
      last <- Gen.oneOf((s: Int) => s.toString, (s: Int) => (s*2).toString)
    } yield new F[Bytes, String] {
      type S = Int

      def start       = init.getBytes("UTF-8").length
      def fold        = fd
      def end(s: Int) = last(s)

      override def toString = "bytes fold"
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

      override def toString = "bytes fold"
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

  implicit def NonEmptyListArbitrary[T : Arbitrary]: Arbitrary[NonEmptyList[T]] =
    Arbitrary {
      for {
        t  <- arbitrary[T]
        ls <- arbitrary[List[T]]
      } yield NonEmptyList(t, ls:_*)
    }

  implicit def IntFoldEqual[U]: Equal[F[Int, U]] = new Equal[F[Int, U]] {
    def equal(a1: F[Int, U], a2: F[Int, U]): Boolean = {
      (a1.run((1 to 3).toList) == a2.run((1 to 3).toList)) &&
        (a1.run(List[Int]()) == a2.run(List[Int]()))
    }
  }

  implicit def StringFoldEqual[U]: Equal[F[String, U]] = new Equal[F[String, U]] {
    def equal(a1: F[String, U], a2: F[String, U]): Boolean =
      (a1.run((1 to 10).toList.map(_.toString)) == a2.run((1 to 10).toList.map(_.toString))) &&
        (a1.run(List[String]()) == a2.run(List[String]()))
  }

  implicit def EqualString: Equal[String] =
    Equal.equalA[String]

  implicit class approxEqual(d1: Double) {
    def ==~(d2: Double) = math.abs(d1 - d2) <= 0.001
  }

  /**
   * FILES
   */
  case class Line(value: String)

  implicit def ArbitraryLine: Arbitrary[Line] =
    Arbitrary(arbitrary[String].filter(l => !Seq("\n", "\r").exists(l.contains)).map(Line.apply))

  def withTempFile(action: File => IO[Prop]) = {
    val file = createTempFile
    file.createNewFile
    try action(file).unsafePerformIO
    finally file.delete
  }

  def withTempFiles2(action: (File, File) => IO[Prop]) = {
    val (file1, file2) = (createTempFile(s"file1-"), createTempFile(s"file2-"))
    try action(file1, file2).unsafePerformIO
    finally { try file1.delete finally file2.delete }
  }

  def withTempDir(action: File => IO[Prop]) = {
    val dir = createTempDir
    try action(dir).unsafePerformIO
    finally deleteDir(dir)
  }

  def createTempFile: File =
    createTempFile("")

  def createTempFile(prefix: String): File = {
    val uuid = UUID.randomUUID
    val fileName = "target/test-foldmspec/"+prefix+uuid.toString
    val tmpFile = new File(fileName)
    if (!tmpFile.getParentFile.exists) tmpFile.getParentFile.mkdirs
    if (tmpFile.exists) tmpFile.delete
    tmpFile
  }

  def createTempDir: File =
    createTempDir("")

  def createTempDir(prefix: String): File = {
    val uuid = UUID.randomUUID
    val dirName = "target/test-foldmspec/"+prefix+uuid.toString
    val tmpDir = new File(dirName)
    if (tmpDir.exists) deleteDir(tmpDir)
    tmpDir
  }

  def deleteDir(dir: File): Unit = {
    val files = dir.listFiles
    Option(files).fold(dir.delete: Unit)(fs => deletePaths(fs.toList))
  }

  def deletePaths(paths: List[File]): Unit = {
    val (dirs, files) = paths.partition(_.isDirectory)
    files.foreach(_.delete)
    dirs.foreach(deleteDir)
  }
}
