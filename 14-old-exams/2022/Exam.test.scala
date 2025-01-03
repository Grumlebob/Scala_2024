/* This file is empty on purpose.   It is added, and configured if you
 * wanted to add your own tests during the exam.  It is not graded and
 * should not be submitted.
 */
package adpro

import org.scalacheck.{Gen, Arbitrary}
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Prop.{forAll, forAllNoShrink, propBoolean}

import PrimesAndLaziness.*
import adpro.ExceptionalOptions.*
import adpro.ApplesToApples.*
import adpro.SizedLists.*
import adpro.SizedLists.SizedList.{Cons, Empty}



object Exam2022AutumnSpec
  extends org.scalacheck.Properties("exam-2022"):



  /** ✅ Q1: SafeTotal Property Test */
  property("SafeTotal should wrap exceptions in None and successful calls in Some") = {
    val safeDiv = SafeTotal[Int, Int](x => 42 / x)

    // Case 1: n == 0 should return None
    val zeroCase = safeDiv(0) == None

    // Case 2: n != 0 should return Some(42 / n)
    val nonZeroCase = forAll(Gen.choose(-1000, 1000).filter(_ != 0)) { (n: Int) =>
      safeDiv(n) == Some(42 / n)
    }

    zeroCase && nonZeroCase
  }


  /** ✅ Q2: headOption Property Test */
  property("headOption should safely return the first element or None") = 
    forAll { (list: List[Int]) =>
      val expected = list.headOption
      headOption(list) == expected
    }

  /** ✅ Q3: primesApart Property Test */
  property("Elements in pairs returned by primesApart differ by n") = 
    forAll(Gen.choose(2, 20).filter(_ % 2 == 0)) { (n: Int) =>
      // Take the first 5 pairs
      val pairs = PrimesAndLaziness.primesApart(n).take(5).toList
      
      // Debug Output
      //println(s"\nTesting primesApart($n):")
      //pairs.foreach { case (p1, p2) => println(s"($p1, $p2)") }

      // Validate the difference
      pairs.forall { case (p1, p2) => 
        p2 - p1 == n
      }
    }


  /** ✅ Q6: Better Type Class Test */
  property("pickBetter should select the better apple") = 
    val bigApple = Apple(1000)
    val smallApple = Apple(10)
    pickBetter(bigApple, smallApple) == bigApple

  /** ✅ Q7: Infix betterThan Test */
  property("Apple should support infix betterThan comparison") = 
    val bigApple = Apple(1000)
    val smallApple = Apple(10)
    bigApple betterThan smallApple

  /** ✅ Q8: SizedList Explicit Type Annotations Test */
  property("SizedList l1 and l2 have correct types and structure") = 
    val l1: SizedList[Int, Inc[Null]] = Cons(41, l0)
    val l2: SizedList[Int, Inc[Inc[Inc[Null]]]] = Cons(3, Cons(1, Cons(4, l0)))
    (head(l1) == 41) && (head(l2) == 3)

  /** ✅ Q9: third function retrieves the third element */
  property("third retrieves the third element from a SizedList with at least three elements") = 
    val l3: SizedList[Int, Inc[Inc[Inc[Null]]]] = Cons(1, Cons(2, Cons(3, Empty)))
    third(l3) == 3

  /** ✅ Q10: append correctly appends an element */
  property("append adds an element to the end of SizedList") = 
    val l: SizedList[Int, Inc[Inc[Null]]] = Cons(1, Cons(2, Empty))
    val appended = append(3, l)
    head(tail(tail(appended))) == 3

