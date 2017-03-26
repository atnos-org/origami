# Origami

[![Join the chat at https://gitter.im/atnos-org/origami](https://badges.gitter.im/atnos-org/origami.svg)](https://gitter.im/atnos-org/origami?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)
[![Build Status](https://travis-ci.org/atnos-org/origami.png)](https://travis-ci.org/atnos-org/origami)

<center><img src="./doc/origami-crane.png" alt="fold" width="150px"/></center>


The ***origami*** project provides "Monadic folds" to process streams of data in a composable fashion.
Monadic folds come in 2 flavors:

 - pure folds: for computing things like `min`, `max`, `average`, `hash`,...
 - effectul folds: for sinking data to a file for example
 
The general form of a `Fold` is
```scala
trait Fold[M[_], A, B] {
  type S

  def start: M[S]
  def fold: (S, A) => M[S]
  def end(s: S): M[B]
}
```

where:
 
 - `M` must have a `Monad` instance
 - `A` is the type of input elements, being fed one by one to the fold
 - `B` is the final result
 - `S` is the type of some internal state
 - `start` is a method to "initialize" the fold
 - `end` is a method to "finalize" the fold
 - `fold` is the method called for each element `A` and current type `S`
   
Folds can be composed to produce "larger" folds, doing several things at the same time. For example:
```scala
import org.atnos.origami._
import org.atnos.origami.fold._
import org.atnos.origami.folds._
import org.atnos.origami.syntax.foldable._
import cats.Eval
import cats.data.EitherT
import cats.implicits._
import java.io.PrintWriter

type Safe[A] = EitherT[Eval, Throwable, A]
def protect[A](a: =>A): Safe[A] = EitherT.right(Eval.later(a))

def saveToFile(path: String): Sink[Safe, Int] =
  bracket(
    // create a new writer
    protect(new PrintWriter(path)))(
    
    // write a new line in the file
    (w, i: Int) => protect { w.write(s"i=$i\n"); w })(
    
    // close the writer
    w => protect(w.close))

val stats: Fold[Safe, Int, ((Int, Int), Double)] =
  (minimumOr(0) <*> maximumOr(Int.MaxValue) <*> averageDouble).into[Safe] <*
    saveToFile("target/readme-example")

val elements = (1 to 10).toList

elements.foldWith(stats).value.value
```

In the example above we create a `stats` fold composed from:

 - 3 pure folds assembled with `<*>` (the `zip` operator)
 - 1 effectful fold `saveToFile` using the `Safe` monad
 
The `Safe` monad is necessary here to use the `bracket` combinator which creates a `Fold` acquiring resources at the 
 beginning of the run and eventually release them. It needs both the ability to deal with errors (with `EitherT`) and to
 delay computations (with `Eval`).

