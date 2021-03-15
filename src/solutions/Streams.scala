package solutions

import u03.Streams._
import u03.Streams.Stream._

object Streams {

  object Stream {

    // NB: ho dovuto impostare Cons su PUBLIC per il case Cons

    // 5
    def drop[A](stream: Stream[A])(n: Int): Stream[A] = (stream,n) match {
      case (Cons(_, tail), n) if n>0 => drop(tail())(n - 1)
      case _ => stream
    }

    // 6
    def constant[A](k: => A): Stream[A] = cons(k, constant(k))

    def constantByIterate[A](k: => A): Stream[A] = iterate(k)(k=>k)

    // 7
    def fibs: Stream[Int] = {
      def _fibs(p: Int, n: Int): Stream[Int] = Cons(()=>p,()=>_fibs(n, p + n))
      _fibs(0, 1)
    }

  }
}