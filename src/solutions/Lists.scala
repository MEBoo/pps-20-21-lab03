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

    def flatMap[A,B](l: List[A])(mapper: A => List[B]): List[B] = l match {
      case Cons(h, t) => append(mapper(h),flatMap(t)(mapper))
      case Nil() => Nil()
    }

    def map[A,B](l: List[A])(mapper: A=>B): List[B] = flatMap(l)(value => Cons(mapper(value),Nil()))

  }
}