package solutions

import org.junit.jupiter.api.Assertions._
import org.junit.jupiter.api.Test

class AllTests {

  import solutions.Lists.List._
  import u03.Lists.List._

  val lst = Cons(10 , Cons(20 , Cons(30 , Nil())))

  // 1.a
  @Test def testListDrop() {
    
    assertEquals(Cons(20, Cons(30, Nil())),drop(lst,1))
    assertEquals(Cons(30, Nil ()),drop(lst,2))
    assertEquals(Nil(),drop(lst,5));
    assertEquals(Nil(),drop(Nil(),1));

  }

  // 1.b
  @Test def testListFlatMap() {

    assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), flatMap( lst )(v => Cons( v+1 , Nil() )) )
    assertEquals(Cons(11, Cons(12, Cons(21, Cons(22, Cons(31, Cons(32, Nil())))))), flatMap ( lst )(v => Cons( v +1 , Cons(v +2 , Nil() ))))
    
    // 1.c
    assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), map( lst )(v => v+1) )

    // 1.d
    assertEquals(Cons(20, Cons(30, Nil())), filter( lst )(_ >=20) )
    assertEquals(Cons(30, Nil()), filter( lst )(_ >=30) )
    assertEquals(Cons(10, Nil()), filter( lst )(_ <= 10) )

  }

  import u02.Optionals.Option._
  // 2
  @Test def testMax() {

    assertEquals(Some(25), max1( Cons(10, Cons(25, Cons(20, Nil()))) ) )
    assertEquals(None(), max1( Nil()) )

    assertEquals(Some(25), max2( Cons(10, Cons(25, Cons(20, Nil()))) ) )
    assertEquals(None(), max2( Nil()) )

  }

  // 3
  import u02.Modules.Person

  @Test def testCourses() {

    assertEquals(Cons("CourseA", Cons("CourseB", Cons("CourseC", Nil()))), getCourses( Cons(Person.Teacher("mario","CourseA"),Cons(Person.Student("mario",2015),Cons(Person.Teacher("luca","CourseB"),Cons(Person.Teacher("matteo","CourseC"), Nil())))) ))

  }

  // 4
  val lstToFold = Cons (3 , Cons(7 , Cons(1 , Cons(5 , Nil () ) ) ) )

  @Test def testFold() {

    assertEquals(-16, foldLeft( lstToFold )(0)(_-_))
    assertEquals(-8, foldRight( lstToFold )(0)(_-_))

    assertEquals(5, foldLeft( Nil() )(5)(_-_))

  }

  // 5
  import solutions.Streams.Stream.drop
  import u03.Streams.Stream

  val s = Stream.take( Stream.iterate(0) ( _ +1 ) )(10)

  @Test def testStreamDrop() {

    assertEquals(Cons(6, Cons(7, Cons(8 , Cons(9, Nil())))), Stream.toList( drop( s )(6) ))

  }

  // 6
  import solutions.Streams.Stream.{constant, constantByIterate}

  @Test def testStreamConstant() {

    val x=5
    assertEquals( Cons (x, Cons (x, Cons (x, Cons (x, Cons (x, Nil ()))))), Stream.toList( Stream.take( constant( x ) )(5) ) )

    assertEquals( Cons (x, Cons (x, Cons (x, Cons (x, Cons (x, Nil ()))))), Stream.toList( Stream.take( constantByIterate( x ) )(5) ) )

  }
  import solutions.Streams.Stream.fibs
  
  @Test def testStreamFibs() {

    assertEquals( Cons(0, Cons(1, Cons(1, Cons(2, Cons(3, Cons(5, Cons(8, Cons(13, Nil ())))))))), Stream.toList( Stream.take( fibs )(8) ) )

  }

}