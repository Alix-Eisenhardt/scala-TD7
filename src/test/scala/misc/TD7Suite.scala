package misc

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * This class implements a ScalaTest test suite for the methods in object `Insertion` that need to be implemented as part of
 * this assignment. A test suite is simply a collection of individual tests for some specific component of a program.
 *
 * A test suite is created by defining a class which extends the type `org.scalatest.FunSuite`. When running ScalaTest, it
 * will automatically find this class and execute all of its tests.
 *
 * Adding the `@RunWith` annotation enables the test suite to be executed inside eclipse using the built-in JUnit test runner.
 *
 * You have two options for running this test suite:
 *
 * - Right-click this file in eclipse and chose "Run As" - "JUnit Test"
 */

@RunWith(classOf[JUnitRunner])
class TD7Suite extends FunSuite {

  import TD7._
  
  test("Test P1"){
    assert(rmCons(List()) === List())
    assert(rmCons(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) === List('a,'b,'c,'a,'d,'e))
    assert(rmCons(List('a,'b,'c,'a,'d,'e)) === List('a,'b,'c,'a,'d,'e))
  }
  
  test("Test P3"){
    assert(encode(List()) === List())
    assert(encode(List('a)) === List((1,'a)))
    assert(encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) === List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e)))
    assert(encode(List('a, 'a, 'a, 'a, 2, 2, 2, 'a, 'a, List(42), "coucou", "coucou", "coucou", "coucou")) === List((4,'a), (3,2), (2,'a), (1,List(42)), (4,"coucou")))
  }

  test("Test P5"){
    assert(rotate(List(),0) === List())
    assert(rotate(List(),2) === List())
    assert(rotate(List(),-2) === List())
    assert(rotate(List(1),0) === List(1))
    assert(rotate(List(1),2) === List(1))
    assert(rotate(List(1),-2) === List(1))
    assert(rotate(List(1,2,3,4,5,6),0) === List(1,2,3,4,5,6))
    assert(rotate(List(1,2,3,4,5,6),2) === List(3,4,5,6,1,2))
    assert(rotate(List(1,2,3,4,5,6),-2) === List(5,6,1,2,3,4))
    assert(rotate(List(1,'a,3,"coucou",5,List(42)),0) === List(1,'a,3,"coucou",5,List(42)))
    assert(rotate(List(1,'a,3,"coucou",5,List(42)),2) === List(3,"coucou",5,List(42),1,'a))
    assert(rotate(List(1,'a,3,"coucou",5,List(42)),-2) === List(5,List(42),1,'a,3,"coucou"))
  }
}

 	