package mjis.util

/** no-op implementation of scala.control.TailCalls for debugging with actual stack traces
  */
object DummyTailCalls {

  class TailRec[A](x: A) {
    def map[B](f: A => B) = f(x)

    def flatMap[B](f: A => TailRec[B]) = f(x)

    def result: A = x
  }

  def done[A](x: A) = new TailRec(x)
  def tailcall[A](f: => TailRec[A]) = f

}