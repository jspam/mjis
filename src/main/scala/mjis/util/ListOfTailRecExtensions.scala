package mjis.util

import scala.util.control.TailCalls._

object ListOfTailRecExtensions {
  implicit class ListOfTailRecExtensions[A](xs: List[A]) {
    /** Successively evaluates TailRec instances over f from left to right.
      * It is guaranteed that all elements and their invocations of f are strictly evaluated
      * in the given order.
      */
    // have to wrap it in tailcall() since the evaluation of xs(0) could recursively call sequenceMap again
    def sequenceMap[B](f: A => TailRec[B]): TailRec[List[B]] = tailcall(xs match {
      case Nil => done(Nil)
      case y :: ys => f(y).flatMap(yResult => ys.sequenceMap(f).map(yResult :: _))
    })
  }
}
