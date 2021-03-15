package solutions

import org.junit.jupiter.api.Assertions._
import org.junit.jupiter.api.Test

class AllTests {

  import u03.Lists.List._
  import solutions.Lists.List._

  val lst = Cons(10 , Cons(20 , Cons(30 , Nil())))

  //1.a
  @Test def testListDrop() {
    
    assertEquals(drop(lst,1), Cons(20, Cons(30, Nil())))
    assertEquals(drop(lst,2), Cons(30, Nil ()))
    assertEquals(drop(lst,5), Nil());
    assertEquals(drop(Nil(),1), Nil());

  }

  //1.b
  @Test def testListFlatMap() {

    assertEquals(Cons (11, Cons(21, Cons (31, Nil ()))), flatMap( lst )(v => Cons ( v+1 , Nil () )) )
    assertEquals(Cons (11, Cons(12, Cons (21, Cons (22, Cons(31, Cons (32, Nil ())))))), flatMap ( lst )(v => Cons ( v +1 , Cons (v +2 , Nil () ))))

    //1.c
    assertEquals(Cons (11, Cons(21, Cons (31, Nil ()))), map( lst )(v => v+1) )

  }


}