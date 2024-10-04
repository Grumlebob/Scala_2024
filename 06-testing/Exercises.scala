// Advanced Programming, A. Wąsowski, IT University of Copenhagen
// Based on Functional Programming in Scala, 2nd Edition

package adpro.lazyList

import org.scalacheck.*
import org.scalacheck.Prop.*
import org.scalacheck.Arbitrary.arbitrary

import lazyList00.* // uncomment to test the book laziness solution implementation
// import lazyList01.* // uncomment to test the broken headOption implementation
// import lazyList02.* // uncomment to test another version

/* Generators and helper functions */

import LazyList.*

/** Convert a strict list to a lazy-list */
def list2lazyList[A](la: List[A]): LazyList[A] = 
  LazyList(la*)

/** Generate finite non-empty lazy lists */
def genNonEmptyLazyList[A](using Arbitrary[A]): Gen[LazyList[A]] =
  for la <- arbitrary[List[A]].suchThat { _.nonEmpty }
  yield list2lazyList(la)
  
/** Generate an infinite lazy list of A values.
  *
  * This lazy list is infinite if the implicit generator for A never fails. The
  * code is ugly-imperative, but it avoids stack overflow (as Gen.flatMap is
  * not tail recursive)
  */
def infiniteLazyList[A: Arbitrary]: Gen[LazyList[A]] =
  def loop: LazyList[A] =
    summon[Arbitrary[A]].arbitrary.sample match
      case Some(a) => cons(a, loop)
      case None => empty
  Gen.const(loop)

/* The test suite */

object LazyListSpec 
  extends org.scalacheck.Properties("testing"):

  // Exercise 1

  property("Ex01.01: headOption returns None on an empty LazyList") = 
    empty.headOption == None

  property("Ex01.02: headOption returns the head of the stream packaged in Some") =

    given Arbitrary[LazyList[Int]] = Arbitrary(genNonEmptyLazyList[Int])

    forAll { (n: Int) => cons(n,empty).headOption == Some(n) } :| "singleton" &&
    forAll { (s: LazyList[Int]) => s.headOption != None }      :| "random" 

  // Exercise 2
  property("Ex02: headOption does not force the tail") = 
    //head is 1, which is not dangerous
    //tail is exception, which is dangerous, so if it is forced we will also get an error!
    val dangerousTail = cons(1, throw new RuntimeException("Tail was forced"))
    //We shouldn't get an error, as only head should be forced.
    dangerousTail.headOption == Some(1)

  // Exercise 3
  property("Ex03: take does not force any heads or tails") =
    //Both head and tail are exceptions, so if forced the test will fail. 
    val dangerousList = cons(throw new RuntimeException("Head forced"), cons(throw new RuntimeException("Tail forced"), empty))
    //We should be able to take, without forcing head or tail
    dangerousList.take(1)
    true // Test passes if no exception is thrown

// Exercise 4
  property("Ex04: take(n) does not force the (n+1)st head") = 
    given Arbitrary[LazyList[Int]] = Arbitrary(genNonEmptyLazyList[Int])

    // Gen.posNum only generates positive integers
    forAll(Gen.posNum[Int], genNonEmptyLazyList[Int]) { (n: Int, lst: LazyList[Int]) =>
    
    // Ensure the LazyList has at least n elements before appending dangerous tail
      if (lst.take(n).toList.size < n) {
        true // Skip test if the list is too small
      } else {
        
        // Create a dangerous tail that throws an exception if forced 
        val dangerousTail = cons(throw new RuntimeException("Tail was forced"), empty)
        //we position the dangerous tail at n+1 (which is what we are testing!)
        val extendedList = lst.append(dangerousTail)

        try
        // Force the first n elements, but not the (n+1)st
          extendedList.take(n).toList
          true // Pass if no exception occurs
        catch
          case _: RuntimeException => false // Fail if tail was forced
    }
  }


  // Exercise 5
  property("Ex05: take(n).take(n) == take(n) (comparing contents with length check)") = 
    given Arbitrary[LazyList[Int]] = Arbitrary(genNonEmptyLazyList[Int])

    forAll(Gen.posNum[Int], genNonEmptyLazyList[Int]) { (n: Int, lst: LazyList[Int]) =>
      val listAsSeq = lst.toList
    
      // Ensure n is less than the length of the LazyList
      if (n <= listAsSeq.length) {
        val firstTake = lst.take(n).toList
        val secondTake = lst.take(n).take(n).toList

        //Should be equal
        firstTake == secondTake
      } else {
        true // If n exceeds length, skip this case as it is irrelevant
    }
  }



  // Exercise 6
  property("Ex06: drop(n).drop(m) == drop(n + m) with length check") = 
    given Arbitrary[LazyList[Int]] = Arbitrary(genNonEmptyLazyList[Int])

    forAll(Gen.posNum[Int], Gen.posNum[Int], genNonEmptyLazyList[Int]) { (n: Int, m: Int, lst: LazyList[Int]) =>
      val listAsSeq = lst.toList
      val totalDrop = n + m
      
      // Ensure n + m does not exceed the length of the LazyList
      if (totalDrop <= listAsSeq.length) {
        // Check that dropping n then m is equivalent to dropping n + m
        lst.drop(n).drop(m).toList == lst.drop(n + m).toList
      } else {
        true // Skip this test case if n + m exceeds the length of the LazyList
      }
  }


  // Exercise 7
  property("Ex07: drop(n) does not force any of the dropped elements with length check") = 
    given Arbitrary[LazyList[Int]] = Arbitrary(genNonEmptyLazyList[Int])

    forAll(Gen.posNum[Int], genNonEmptyLazyList[Int]) { (n: Int, lst: LazyList[Int]) =>
      val listAsSeq = lst.toList
    
      // Ensure n does not exceed the length of the LazyList
      if (n <= listAsSeq.length) {
        
        // Create a dangerous tail that throws an exception if forced
        val dangerousList = cons(throw new RuntimeException("Head forced"), lst)

        try
          // Drop n elements and ensure none of the dropped elements are forced
          dangerousList.drop(n)
          true // Test passes if no exception is thrown
        catch
          case _: RuntimeException => false // Test fails if any dropped element is forced
      } else {
        true // Skip the test if n exceeds the length of the LazyList
    }
  }


  // Exercise 8
  property("Ex08: Mapping identity over a LazyList should not change the list. map(identity) == l") = 
    given Arbitrary[LazyList[Int]] = Arbitrary(genNonEmptyLazyList[Int])

    forAll { (lst: LazyList[Int]) =>
      // Convert both the original LazyList and the mapped LazyList to a List for comparison
      //So we compare the values, instead of the structure.
      lst.map(identity).toList == lst.toList
    }


  // Exercise 9
  property("Ex09: map terminates lazily on infinite lists") = 
    // Generate an infinite list
    val infiniteList = infiniteLazyList[Int].sample.get
    //Map elements to be incremented by 1
    infiniteList.map(_ + 1)
    //If it terminates, we will reach this true.
    true

  // Exercise 10.01
  property("Ex10.01: append with empty list returns the original list (comparing contents)") = 
    given Arbitrary[LazyList[Int]] = Arbitrary(genNonEmptyLazyList[Int])

    forAll { (lst: LazyList[Int]) =>
      // Convert the original LazyList and the result of appending `empty` to `List` for comparison
      lst.append(empty).toList == lst.toList
  }


  // Exercise 10.02
  property("Ex10.02: Appending two lists should preserve the order of elements (comparing contents)") = 
    given Arbitrary[LazyList[Int]] = Arbitrary(genNonEmptyLazyList[Int])

    forAll { (lst1: LazyList[Int], lst2: LazyList[Int]) =>
      // Convert both lists and the result of the append operation to List for comparison
      lst1.append(lst2).toList == (lst1.toList ++ lst2.toList)
  }
