package solutions

import org.junit.jupiter.api.Assertions._
import org.junit.jupiter.api.Test

class AllTests {

  import u03.Lists.List._
  import solutions.Lists.List._

  val lst = Cons(10 , Cons(20 , Cons(30 , Nil())))

  @Test def testListDrop() {
    
    assertEquals(drop(lst,1), Cons(20, Cons(30, Nil())))
    assertEquals(drop(lst,2), Cons(30, Nil ()))
    assertEquals(drop(lst,5), Nil());
    assertEquals(drop(Nil(),1), Nil());

  }
}