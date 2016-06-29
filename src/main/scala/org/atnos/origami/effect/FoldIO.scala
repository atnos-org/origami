package org.atnos
package origami
package effect

import java.io._

import FoldId.Bytes

import scalaz._, Scalaz._, effect._
import FoldM._
import SafeT._
import FoldIOTypes._

trait FoldIO extends FoldIOTypes with FoldIOFunctions with FoldIOImplicits
object FoldIO extends FoldIO

trait FoldIOTypes {

  type FoldIO[A, B] = FoldM[IO, A, B]

  type FoldSafeTIO[A, B] = FoldM[SafeTIO, A, B]

  // the sink type is safe
  type Sink[A] = FoldSafeTIO[A, Unit]
}

object FoldIOTypes extends FoldIOTypes

trait FoldIOFunctions {
  /** @return an output stream sink */
  def outputStreamSink[T](out: OutputStream)(write: (OutputStream, T) => Unit): Sink[T] =
    sinkIO[T, OutputStream](SafeT.point(out))(write)

  /** @return an output file stream sink */
  def fileSink[T](file: File)(write: (OutputStream, T) => Unit): Sink[T] =
    fileSink(file.getPath)(write)

  /** @return an output file stream sink */
  def fileSink[T](file: String)(write: (OutputStream, T) => Unit): Sink[T] =
    sinkIO[T, OutputStream](createOutputStream(file))(write)

  /** @return an output stream sink for string lines */
  def fileUTF8LineSink(file: File): Sink[String] =
    fileUTF8LineSink(file.getPath)

  /** @return an output stream sink for string lines */
  def fileUTF8LineSink(file: String): Sink[String] =
    sinkIO[String, PrintStream](createPrintStream(file))((s, t) => s.println(t))

  /** @return an output stream sink for string lines */
  def bytesSink[T](file: File): Sink[Bytes] =
    bytesSink(file.getPath)

  /** @return an output stream sink for string lines */
  def bytesSink[T](file: String): Sink[Bytes] =
    sinkIO[Bytes, OutputStream](createOutputStream(file))((s, t) => s.write(t._1, 0, t._2))

  def sinkIO[T, R](init: SafeTIO[R])(fd: (R, T) => Unit): Sink[T] =
    sink[T, R, IO](init)(fd)

  def sink[T, R, M[_] : Monad : Catchable](init: SafeT[M, R])(fd: (R, T) => Unit): SinkM[SafeT[M, ?], T] = new FoldM[SafeT[M, ?], T, Unit] {
    type S = R
    def start = init
    def fold = (s: S, t: T) => { fd(s, t); s }
    def end(s: S) = SafeT.point(())
  }

  def createOutputStream(file: String): SafeTIO[OutputStream] = {
    lazy val s = {
      val f = new File(file)
      if (f.getParentFile != null) f.getParentFile.mkdirs
      new BufferedOutputStream(new FileOutputStream(f)): OutputStream
    }
    IO(s) `finally` IO(s.close)
  }

  def createPrintStream(file: String): SafeTIO[PrintStream] = {
    lazy val s = {
      val f = new File(file)
      if (f.getParentFile != null) f.getParentFile.mkdirs
      new PrintStream(f, "UTF-8")
    }
    IO(s) `finally` IO(s.close)
  }
}

