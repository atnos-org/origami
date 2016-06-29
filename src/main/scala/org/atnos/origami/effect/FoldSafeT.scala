package org.atnos
package origami
package effect
/*
import scalaz._, Scalaz._
import FoldM._
import scalaz.concurrent.Task
import SafeT._
import scalaz.effect._

/**
 * Specialized instances for the SafeT monad
 */
trait FoldSafeT {

  private implicit def SafeTMonadM[M[_] : Monad : Catchable] =
    SafeT.SafeTMonad[M]

  /** Natural transformation from Id to SafeT[M, ?] */
  def IdSafeTNaturalTransformation[M[_] : Monad : Catchable]: Id ~> SafeT[M, ?] =
    FoldM.IdMonadNaturalTransformation[SafeT[M, ?]](SafeTMonad)

  implicit val IdSafeTTaskNaturalTransformation: Id ~> SafeTTask =
    IdSafeTNaturalTransformation[Task]

  implicit def IdSafeTIONaturalTransformation: Id ~> SafeTIO =
    IdSafeTNaturalTransformation[IO]

  implicit val TaskToSafeTTaskNaturalTransformation: Task ~> SafeTTask = new (Task ~> SafeTTask) {
    def apply[A](i: Task[A]): SafeTTask[A] = SafeT.point(i.run)
  }

}

object FoldSafeT extends FoldSafeT
*/
