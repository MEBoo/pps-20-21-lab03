package solutions

import u03.Lists.List._
import u03.Lists._

object Lists {

  object List {

    // 1.a
    @annotation.tailrec
    def drop[A](l: List[A], n: Int): List[A] =  (l, n) match {
      case (Cons(h, t), n) if n>0 => drop(t,n-1)
      case _ => l
    }

    // 1.b
    def flatMap[A,B](l: List[A])(mapper: A => List[B]): List[B] = l match {
      case Cons(h, t) => append(mapper(h),flatMap(t)(mapper))
      case Nil() => Nil()
    }

    // 1.c
    def map[A,B](l: List[A])(mapper: A=>B): List[B] = flatMap(l)(value => Cons(mapper(value),Nil()))

    // 1.d
    def filter[A](l1: List[A])(pred: A=>Boolean): List[A] = flatMap(l1)( v => pred(v) match {
      case true => Cons(v,Nil())
      case _ => Nil()
    })

    // 2
    import u02.Optionals._
    import u02.Optionals.Option._

    @annotation.tailrec
    def max1(l: List[Int]): Option[Int] = l match {
      case Cons(h, t) => t match {
        case Cons(h_next, _) if (h_next>h) => max1(t)
        case Cons(_, t_next) => max1(Cons(h,t_next))
        case _ => Some(h)
      }
      case Nil() => None()
    }

    def max2(l: List[Int]):Option[Int] = {

      @annotation.tailrec
      def _max(l: List[Int], max:Int):Int = l match {
        case Cons(h, t) if (h>max) => _max(t,h)
        case Cons(_, t) => _max(t,max)
        case _ => max
      }

      l match {
        case Nil() => None()
        case _ => Some(_max(l,0))
      }
    }

    // 3
    import u02.Modules.Person
    import u02.Modules.Person._

    def getCourses(l: List[Person]):List[String] = flatMap(l)( p => p match {case Teacher(_,c) =>Cons(c,Nil()) case _ => Nil()} )

    // 4
    @annotation.tailrec
    def foldLeft(l:List[Int])(v:Int)(operator: (Int,Int)=>Int ):Int = l match {
      case Cons(h,t) => foldLeft(t)(operator(v,h))(operator)
      case _ => v
    }

    def foldRight(l:List[Int])(v:Int)(operator: (Int,Int)=>Int ):Int = l match {
      case Cons(h,t) => operator(h,foldRight(t)(v)(operator))
      case _ => v
    }
  }
}