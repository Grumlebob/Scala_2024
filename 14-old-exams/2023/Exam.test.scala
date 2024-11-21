/* This file is empty on purpose.   It is added, and configured if you
 * wanted to add your own tests during the exam.  It is not graded and
 * should not be submitted.
 */
package adpro

import org.scalacheck.{Gen, Arbitrary}
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Prop.{forAll, forAllNoShrink}
import laziness.LazyList

import LazyList.*
import adpro.Streaming.fViaFold
import adpro.Parsing.longestLine
import adpro.parsing.Sliceable.run

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

object ExamSpec
  extends org.scalacheck.Properties("exam-2023-autumn"):

  property("A test that always passes (a sanity check)") = 
    forAll { (n: Int) => n == n }

  property("Ex1.0 fViaFold counts odds") =
    //given Arbitrary[LazyList[Int]] = Arbitrary(genNonEmptyLazyList[Int])
    Streaming.fViaFold(LazyList(0, 1, 2, 3, 4)) == 2 &&
    Streaming.fViaFold(LazyList(0, 1)) == 1 &&
    Streaming.fViaFold(LazyList(0)) == 0

  property("Ex2.0 longestLine counts longest line") =
    Parsing.longestLine.run("1,2,3,4,5\n1,2,3") match
      case Right(a) => a == 5
      case _ => false

  property("Ex3.1 allLinesTheSame false for not same") =
    Parsing.allLinesTheSame.run("1,2,3,4,5\n1,2,3") match
      case Right(a) => a == false
      case _ => true
  property("Ex3.1 allLinesTheSame true for same") =
    Parsing.allLinesTheSame.run("1,2,3\n1,2,3") match
      case Right(a) => a == true
      case _ => false
  
  property("Ex4.0 game makes sense") =
    Game.aliceFraction == 0
    
      
    
end ExamSpec

object NullUpdatesSpecObj
  extends RL.NullUpdatesSpec(update = RL.update, "studentrl") {}
