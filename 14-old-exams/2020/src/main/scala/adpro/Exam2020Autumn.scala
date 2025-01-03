/************************************************************************
 * Final Exam: Advanced Programming by Andrzej Wąsowski
 * IT University of Copenhagen, Autumn 2020: 6 January 2021, 9:00
 *
 * Your Full Name: ___
 * Your ITU email account: ___
 *
 * The exam consists of 9 questions to be solved within 4 hours.
 *
 * It is possible  to answer later questions, missing  the answers for
 * the previous ones, but it is recommended to answer in order.
 *
 * You can use  any function from the course  (textbook, exercises) in
 * the  solutions, as  well as  standard library  functions.  You  can
 * access any written or electronic material, also online, but you are
 * not allowed to communicate with anybody during the exam.
 *
 * By  submitting, you  declare  to have  solved  the problems  alone,
 * without communicating with anybody.
 *
 * SUBMISSION
 *
 * Solve the tasks in the file 'Exam2020Autumn.scala' (this file) found
 * in the zip archive made available on LearnIt.
 *
 * Fill in your name and your ITU email above, in the top of the file.
 *
 * Submit this file and only this  file to learnIT.  Do not convert to
 * any  other  format than  .scala.   Do  not  submit the  entire  zip
 * archive. Do not  reorder the  answers, and  do not  remove question
 * numbers from the file.  The only accepted file format is '.scala'.
 *
 * Keep the solutions within 80 columns width to make grading easier.
 *
 * ADVICE
 *
 * The  answers  will   be  graded  manually. We  will   focus  on  the
 * correctness of ideas and the use  of the course concepts. We will be
 * permissive on  minor issues  such as semicolons,  other punctuation,
 * small deviations  in function  names, switching between  curried and
 * not  curried arguments,  etc.  We  will not  check whether  the type
 * inference succeeds.   It suffices  that a  human reader  could infer
 * types.
 *
 * We do not recommend solving questions to the point when they compile
 * and pass tests.  Dependency problems  and other technical issues can
 * take a lot of time, so only do this, once you are done with drafting
 * all answers.
 *
 * Nevertheless, if  you do compile,  you can use the  'build.sbt' file
 * provided  in the  zip  archive linked  above. It  has the  necessary
 * library dependencies  configured. The zip archive also  contains the
 * course libraries that the solutions depend on.
 *
 * The  percentage  at  the   beginning  of  each  question  indicates
 * difficulty of the question, and will be used as guide in grading.
 *
 * Good luck!
 */

package adpro

import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks._
import org.scalatest.matchers.should.Matchers._
import org.scalactic.Equality


object Q1 { // 10%

  /**
   * BASIC RECURSION, EXCEPTIONS, AND REFERENTIAL TRANSPARENCY
   *
   * The following function  'smash' makes a list  thicker and shorter
   * by turning into a list of  pairs.  It fails with an exception, if
   * the list it receives has an  odd number of elements, as it cannot
   * smash the last odd element.
   */

  def smash[A] (l: List[A]): List[(A,A)] =
    l match {
      case List () =>
        Nil

      case h1 :: h2 :: tl =>
        h1 -> h2 :: smash (tl)

      case _ =>
        throw new NoSuchElementException ()
    }

  /**
   * Examples :
   *
   * smash (List (1,2,3,4)).size should
   *   be (2)
   *
   * smash (List (1,2,3,4)) should
   *   be (List (1 -> 2, 3 -> 4))
   *
   * Write a function 'smashOption'  that is referentially transparent
   * and  achieves  the  corresponding functionality  purely,  in  the
   * Option monad.  Include the return type in the function signature:
   */

  def smashOption[A] (l: List[A]): Option[List[(A,A)]] = 
    l match {
      case Nil =>
        Some (Nil)

      case h1 :: h2 :: tl =>
        Some (h1 -> h2 :: smash (tl))

      case _ =>
        None
    }

}



object Q2 { // 10%

    /**
     * TYPE CLASSES AND TYPE CONSTRAINTS
     *
     * Another way to  handle errors in smash is to  make the function
     * total.  This could be done by assuming that the element type is
     * 'splittable', so  that we can split  an element in half,  if we
     * are left with an odd one.
     *
     * For example, let's  imagine that a Float can  be split dividing
     * the  value  into  two.   Then  smash  could  be  producing  the
     * following:
     *
     *   smash (List[Double] (1,2,3)) == List (1 -> 2, 1.5 -> 1.5)
     *
     * For this we need to assume  that the type variable A represents
     * a splittable type.   Let's say that a type is  splittable if it
     * has an instance of the type class Splittable implemented by the
     * following trait:
     */

    trait Splittable[A] {
      def split (a: A): (A,A)
    }

    /**
     * Implement a new version of smash  that is total and smashes all
     * values of List[A] but requires that A is splittable
     */

    def smash[A: Splittable] (l: List[A]): List[(A,A)] = 
      l match {
        case Nil =>
          Nil

        case h1 :: h2 :: tl =>
          h1 -> h2 :: smash (tl)

        case h1 :: tl =>
          val (h2,h3) = implicitly[Splittable[A]].split (h1)
          h2 -> h3 :: smash (tl)
      }

  }



object Q3 { // 10%

  import Q2._

  /**
   * TYPE CLASS INSTANCES
   *
   * Implement two instances of the Splittable type class:
   *
   * - For Double, splitting a number into two, dividing by 2.0
   * - For List[Int], splittling a list by putting all elements on odd
   *   positions in the left list,  and all elements from even positions
   *   in the right list
   */

  implicit val splitDouble = new Splittable[Double] {
    def split (a: Double): (Double,Double) = (a/2.0, a/2.0)
  }


  implicit lazy val splitListInt =
    new Splittable[List[Int]] {

      def split (l: List[Int]) = {
        val lidx = l.zipWithIndex
        val odd = l.filter { case (_,i) => i % 2 == 1 }
        val even = l.filter { case (_,i) => i % 2 == 0 }
        (odd.unzip._1, even.unzip._2)
      }
    }

}



object Q4 { // 15%

  import Q2._ // Assuming `smash` was implemented in Q2
  import Q3._ // Assuming Foldable structure or other necessary helpers
  import org.scalatest.freespec.AnyFreeSpec
  import org.scalatest.matchers.should.Matchers
  import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
  import org.scalacheck.Gen
  import org.scalacheck.Arbitrary.arbitrary


  /**
   * PROPERTY-BASED TESTING
   *
   * We want to test your smash from Q2.  Write the following tests:
   *
   * - The size of a smashed list is half of the original list size
   *   before smashing, for any list with even number of elements
   *
   * - The size of the smashed list is the size of the original list
   *   divided by half (integer division) and increased by one, for
   *   lists with odd number of elements.
   *
   * Choose the concrete type to test on, so that you use one of the
   * instances created by you in Q3 above.
   *
   * If you did not  solve Q2 or Q3, write the  test for the example
   * implementation of smash from Q1.
   */

  class SmashSpec
      extends AnyFreeSpec
      with Matchers
      with ScalaCheckPropertyChecks {

    // Generator for non-empty lists
    val evenListGen: Gen[List[Int]] = Gen.choose(0, 100).flatMap(n => Gen.listOfN(n * 2, arbitrary[Int]))
    val oddListGen: Gen[List[Int]] = Gen.choose(0, 100).flatMap(n => Gen.listOfN(n * 2 + 1, arbitrary[Int]))

    "The size of a smashed list should be half the size of an even-length list" in {
      forAll(evenListGen) { (list: List[Int]) =>
        whenever(list.size % 2 == 0) {
          val smashed = smash(list)
          smashed.size shouldEqual list.size / 2
        }
      }
    }

    "The size of a smashed list should be (size / 2 + 1) for an odd-length list" in {
      forAll(oddListGen) { (list: List[Int]) =>
        whenever(list.size % 2 == 1) {
          val smashed = smash(list)
          smashed.size shouldEqual (list.size / 2 + 1)
        }
      }
    }
  }


}



object Q5 { // 5%

  import adpro._

	/**
   * LAZY STREAMS
   *
   * Consider a (possibly infinite) lazy stream of Either[A,B] values,
   * where  the left  component represents  a failure,  and the  right
   * component represents  a success.   Assume the  following liveness
   * property: there is always a success after several failures in the
   * stream. Convert  this stream  into  a stream  of successes,  that
   * maintains its laziness.
	 *
   * You can use  any function of the  book implementation
   * of streams.
	 */

	def successes[A,B] (results: Stream[Either[A,B]]): Stream[B] = 
    results flatMap { _ match {
      case Right (b) => Stream (b)
      case Left (b) => Stream ()
    } }


}



object Q6 { // 10%

	/**
   * REFLECTION ON LAZY STREAMS
   *
   * Explain what  problems could  happen if  the stream  of 'results'
   * above does not guarantee liveness,  i.e. there could be a success
   * after which there are infinitely  many failures.  Explain how the
   * computation of 'successes' fails  (how the problem may manifest)?
   * Explain what  is the fault  (what mechanism causes  the failure)?
   * The indicative size of the answer is 5-10 lines, more and less is
   * allowed.
   *
   * ???
   * 
   *    * Possible answer for successes1 (from Jonas):
   *
   * If  there  are  infinitely  many subsequent  failures,  then  the
   * computation will  not terminate. It will  get stuck on  trying to
   * obtain  the head  of  the stream  of  successes, while  iterating
   * through the failures.
   *
   * More  precisely, the  problem will  appear in  flatMap, when  its
   * result is  forced to deliver  the next element.  This  will cause
   * flatMap to  continue folding producing empty  streams forver.  Of
   * course, this  does not  occur until we  actually evaluate  it, as
   * long as it  remains unevaluated (lazy), we are not  aware of this
   * problem.
   */
   */

}



object Q7 { // 10%

  /**
   * EXTENSION METHODS / PIMP MY LIBRARY PATTERN
   *
   * We are  interested in  implementing a type  of vectors  of double
   * numbers. We decided that we will represent these vectors as lists
   * of doubles wrapped  in an option, so that we  can capture failure
   * of vector operations:
   */

  type VectorD = Option[List[Double]]

  /** For instance (read quickly, this is just an example): */

  def vector_plus2 (v1: VectorD, v2: VectorD): VectorD =
    for {
      l1 <- v1
      l2 <- v2
      if l1.size == l2.size
    } yield (l1 zip l2) map { case (x,y) => x + y }

  /**
   * Use the pimp-my-library pattern to add support for element access
   * to vectors, so that  if 'v' is a VectorD we can  write 'v (0)' to
   * access the first element, etc.   The access 'v (i)' should return
   * a value  of type  Option[Double]. It should be  'Some' containing
   * the value stored  at the ith position, or 'None'  if the index is
   * out of bounds.
   */

  // Add extension methods for accessing elements to VectorD
  implicit class VectorDExtensions(v: VectorD) {
    def apply(i: Int): Option[Double] =
      v match {
        case Some(l) if i >= 0 && i < l.size => Some(l(i))
        case _ => None
      }

}



object Q8 { // 10%

  /**
   * POLYMORPHIC RECURSION
   *
   * Consider  the following  type  Box  that defines  polymorphically
   * recursive lists. In Boxes of type A we  have a head of type A and
   * a tail of type Box[List[A]].  Note that the question has 3 parts.
   */

  sealed trait Box[+A]

  case class Pack[A] (
    hd: A,
    tl: Box[List[A]]
  ) extends Box[A]

  case object End
  extends Box[Nothing]

  /**
   * An example value, storing these numbers in order:
   * 1, 2, 3, 4, 5, 1, 2, 3, 4, 42, 42, 42, 42
   */
  val box: Box[Int]  =
    Pack(
      1,
      Pack (
        List (2,3,4,5),
        Pack (
          List (List (1), List(2,3,4), List (42,42,42,42)),
          End
        )
      )
    )

  /**
   * 1. Write a polymorphic function flattenBox  that, for any type A,
   * converts  a Box[A]  value into  a  usual Scala  List[A] value  by
   * traversing the elements in order.
   */

  def flattenBox[A] (b: Box[A]): List[A] = 
    b match {
      case End =>
        Nil

      case Pack (hd, tl) =>
        hd :: flattenBox (tl).flatten
    }

  /**
   * 2. Which call in your solution is polymorphically recursive?  Add
   *    a type  annotation on the polymorphically  recursive call, and
   *    explain  in  English  below  which  call  it  is  and  why  it
   *    is  polymorphically recursive  (how  can you  see  that it  is
   *    polymorphically recursive).  Expected size 4-5 lines (more and
   *    less is allowed)
   *
 /**
   *  A possible  answer: the  type annotation has  been added  to the
   *  polymorphically recursive  call in line  581 above (the  call in
   *  the Pack  case). The call  is polymorphically  recursive because
   *  the type argument  changes from A to List[A]  between the caller
   *  and the callee.
   */
   */

  /**
   * 3. Consider the Box 'b' below. What is the result of running flattenBox on
   *    it? Write the answer and a short explanation in English why.
   *
   * List (List (2,5), List (1), List (3,4), List (42,42,42))
   *
   * This is the result because 'b' is a boc of type Box[List[Int]] so
   * the  basic  elements of  its  flat  representation are  lists  of
   * integers.
   */

  val b = Pack (
        List (2,5),
        Pack (
          List (List (1), List(3,4), List (42,42,42)),
          End
        ))
}



object Q9 { // 15%

  import adpro.Parsing._
  import adpro.Parsing.MyParsers._

  /**
   * Use the parser  combinator library from the textbook  to write a
   * parser that  parses CSV files  of integer numbers (and  no other
   * types) The type produced should be List[List[Int]].
   *
   * Example input:
   *
   * 1,2,3 , 5,4
   * 42,42
   *
   * Each line  in the  file contains  integers separated  with commas
   * (',') and possibly  white space (tab or  space character).  Lines
   * are  separated  by  '\n'.   The  last line  may  or  may  not  be
   * terminated by '\n'.   There can be no empty lines  (except for in
   * the end  of the  file, if  the last line  is terminated  with the
   * optional \n character).   An empty file parses to an empty list.
   */

  val WS: Parser[String] = """(\t| )+""".r

  val NL: Parser[String] = string("\n")

  val INT: Parser[Int] = """(\+|-)?[0-9]+""".r.map { _.toInt }

  val commaSeparatedInts: Parser[List[Int]] =
    { WS.? |* INT ** ( "," |* WS.? |* INT).* *| WS.? }
      .map { case (h,t) => h::t }

  lazy val csvInt: Parser[List[List[Int]]] =
    { commaSeparatedInts ** { ( NL |* commaSeparatedInts ) }.* *| NL.? }
      .map { case (h,t) => h::t }

  lazy val parser: Parser[List[List[Int]]] =
    { csvInt.? } map { _.getOrElse (Nil) }

}
