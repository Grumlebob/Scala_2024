/* This file is empty on purpose.   It is added, and configured if you
 * wanted to add your own tests during the exam.  It is not graded and
 * should not be submitted.
 */
package adpro

import org.scalacheck.{Gen, Arbitrary}
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Prop.{forAll, forAllNoShrink}
import org.scalacheck.Prop
import org.scalactic.TripleEquals.*


object ExamSpec extends org.scalacheck.Properties("exam-2024-autumn"):

  property("A test that always passes (a sanity check)") =
    forAll { (n: Int) => n == n }

end ExamSpec

object StreamingSpec extends org.scalacheck.Properties("lazylist__"):

  /** Fixed input test cases **/
  property("00 fib(0) returns 0") = fib(0) === 0
  property("01 fib(1) returns 1") = fib(1) === 1

  /** Random input tests **/
  property("02 fib(n) satisfies recurrence relation") = 
    forAll(Gen.choose(2, 50)) { n =>
      fib(n) === fib(n - 1) + fib(n - 2)
    }

  /** LazyList behavior tests **/
  given arbLazyList[A: Arbitrary]: Arbitrary[LazyList[A]] =
    Arbitrary(summon[Arbitrary[List[A]]].arbitrary.map(LazyList(_*)))

  property("03 LazyList generates expected sequence") =
    forAll(Gen.choose(0, 10)) { start =>
      LazyList.from(start).take(5).toList == List(start, start + 1, start + 2, start + 3, start + 4)
    }

  property("04 LazyList.take returns specified number of elements") =
    forAll(Gen.choose(1, 10)) { n =>
      LazyList.from(1).take(n).toList.length == n
    }

end StreamingSpec
