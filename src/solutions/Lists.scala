package solutions

import u03.Lists._
import u03.Lists.List._

object Lists {

  object List {

    @annotation.tailrec
    def drop[A](l: List[A], n: Int): List[A] =  (l, n) match {
      case (Cons(h, t), n) if n>0 => drop(t,n-1)
      case _ => l
    }
    
  }
}