/********************************
 * Final Exam: Advanced Programming by Andrzej Wąsowski
 * IT University of Copenhagen, Autumn 2020: 10 January 2022, 15:00
 *
 * The exam consists of 13 questions to be solved within 4 hours.
 *
 * You can use  any function from the course  (textbook, exercises) in
 * the solutions, as  well as  standard library  functions.  You  can
 * access any written or electronic material, also online, but you are
 * not allowed to communicate with anybody.  By  submitting, you
 * legally declare to have solved the problems alone, without
 * communicating with anybody.
 *
 * Solve the tasks in the file 'Exam.scala' (this file) found in the
 * zip archive made available on LearnIt.
 *
 * Submit this file and only this file to learnIT. Do not convert to
 * any other format than .scala.  Do not submit the entire zip
 * archive. Do not  reorder the  answers, and  do not  remove question
 * numbers from the file.  The only accepted file format is '.scala'.
 *
 * Keep the solutions within 80 columns width to make grading easier.
 *
 * The  answers  will   be  graded  manually. We  focus  on  the
 * correctness of ideas, the use  of concepts, clarity, and style. We
 * are permissive on minor issues  such as semicolons,  commas, other
 * punctuation, small deviations  in function  names, switching
 * between  curried and not curried arguments,  etc.  We  will not
 * check whether  the type inference succeeds.   It suffices  that a
 * human reader  could infer types.
 *
 * We do not recommend solving questions to the point when they
 * compile and pass tests.  Dependency problems  and other technical
 * issues can take a lot of time, so only do this, once you are done
 * with drafting all answers.  If  you do compile,  you can use the
 * 'build.sbt' file provided  in the  zip  archive linked  above. It
 * has the  necessary library dependencies  configured, and the
 * source files from the semester are included.
 *
 * The  percentage  at  the   beginning  of  each  question  will be
 * used as a soft indicative weight in grading.
 *
 * Good luck!
 */

package adpro

import org.scalacheck.{Arbitrary, Gen, Prop}
import org.scalacheck.Arbitrary.*
import org.scalactic.Equality
import Arbitrary.*, Prop.*

import fpinscala.answers.laziness.LazyList
import fpinscala.answers.state.*
import fpinscala.answers.monoids.Foldable
import fpinscala.answers.parallelism.Par
import fpinscala.answers.monads.Monad


object Q1:

  /*** WARM-UP
   *
   * Consider the following example of a simple Java class hierarchy.
   * The example is written using Scala syntax so that we do not have
   * to mix languages in the exam.  Recall that in Java all method
   * calls are virtual, so dynamically dispatched.
   *
   * abstract class Printable: 
   *   def hello(): String
   *
   * class Triangle extends Printable:  
   *   override def hello(): String = "triangle"
   *
   * class Square extends Printable: 
   *   override def hello(): String = "square"
   *
   * Now the following is an ADT in Scala, that realizes the same hierarchy:
   */

  enum Printable:
    case Triangle extends Printable
    case Square extends Printable

  /* Q1. (10 %)
   *
   * Write a function `hello` that uses pattern matching and achieves
   * the corresponding effect as calling the method hello in Java
   * implementation.  Uncomment the definition and fill in the gaps.
   */

  /**
   * Example Input:
   * val shape1 = Printable.Triangle
   * val shape2 = Printable.Square
   *
   * Step 1: Match on `shape1` (Printable.Triangle)
   * - Case Printable.Triangle matches, returns "triangle"
   *
   * Step 2: Match on `shape2` (Printable.Square)
   * - Case Printable.Square matches, returns "square"
   *
   * Final Output:
   * hello(Printable.Triangle) -> "triangle"
   * hello(Printable.Square) -> "square"
   */
  def hello(p: Printable): String = 
    p match
      case Printable.Triangle => "triangle"
      case Printable.Square => "square"

end Q1



object Q2:

  /*** SEQUENCE AND EITHER
   *
   * Q2. (10%)
   *
   * Implement the `sequence` function for Either. The behavior
   * should be like with `sequence` for Option: at least one failure
   * (Left) in the input list should result in a failure overall, a
   * single Left. Otherwise return the Right values from the input
   * list wrapped in a single Right. For full points make sure that
   * the error value returned, if any, is the *last* error value seen
   * on the input list.
   */

  /**
   * sequence Function Explanation:
   * This function converts a list of Either values into a single Either.
   * If any element is Left, the function returns the last encountered Left value.
   * Otherwise, it returns a Right containing a list of extracted Right values.
   *
   * Example Input:
   * List(Right(1), Right(2), Left("error"), Right(3))
   *
   * Step 1: Start with initAcc = Right(Nil)
   * Step 2: Define folder function:
   * - Case Right(a): prepend to accumulator.
   * - Case Left(err): return Left(err)
   *
   * Step 3: Apply foldRight.
   *
   * Final Output:
   * Left("error")
   */
  def _sequence[Err, A](list: List[Either[Err, A]]): Either[Err, List[A]] =
    
   //As we have not failed yet.
    val initAcc: Either[Err, List[A]] = Right(Nil)
    
    def folder(element: Either[Err, A], acc: Either[Err, List[A]]): Either[Err, List[A]] =
      for
        listContent <- acc
        unpackedElement  <- element
      yield unpackedElement :: listContent
    
    list.foldRight(initAcc)(folder)

  def sequence[Err, A](list: List[Either[Err, A]]): Either[Err, List[A]] =
    // As we have not failed yet.
    val initAcc: Either[Err, List[A]] = Right(Nil)
    
    def folder(element: Either[Err, A], acc: Either[Err, List[A]]): Either[Err, List[A]] =
      acc match
        //We preserve the last error encountered.
        case Left(lastError) => Left(lastError)
        //If we have not failed yet. We check next element
        case Right(accList) =>
          
          element match
            // Overwrite with the latest error
            case Left(currentError) => Left(currentError)
            // Forward the list with the new element
            case Right(value) => 
              val updatedList = value :: accList
              Right(updatedList)

    list.foldRight(initAcc)(folder)    

end Q2



object Q3:

  /*** TYPING
   *
   * Imagine that we want to implement `sequence` with Either not
   * using List but any collection F[_] for which we know that it is
   * Foldable.
   *
   * Q3. (10%)
   *
   * Write the type signature for this new function 'sequence'.
   *
   * Do not implement the function, just put '???' in the body.
   */

  /**
   * sequence Function Explanation:
   * This function generalizes `sequence` to any Foldable structure.
   *
   * * Type Parameters:
   * - `F[_]`: A higher-kinded type representing a foldable structure.
   * - `Err`: The type of error used in the `Either`.
   * - `A`: The type of successful value in the `Either`.
   *
   * Context Bound:
   * - `Foldable[F]`: Ensures that `F` supports fold operations.
   * 
   * Example Input:
   * Foldable(List(Right(1), Right(2), Left("error")))
   *
   * Step 1: Start with initAcc = Right(empty structure of F)
   * Step 2: Define folder function:
   * - Case Right(a): Add to structure.
   * - Case Left(err): Return Left(err)
   *
   * Step 3: Apply foldRight.
   *
   * Final Output:
   * Left("error")
   */
  def sequence[F[_]: Foldable, Err, A](foldableList: F[Either[Err, A]]): Either[Err, F[A]] = ???

end Q3



object Q4:

  /*** RANDOM VALUE GENERATORS
   *
   * Recall that the type State.Rand[A] is defined as follows:
   *
   * type Rand[A] = State[RNG, A]
   *
   * Q4. (10%)
   *
   * Implement a pure generator of triples, where the first two
   * components are random integers 'a' and 'b', whereas the third
   * component 'x' is a random double number between them, so the
   * following constraint is satisfied:
   *
   *     a <= x <= b
   */

  type Rand[A] = State[RNG, A]

  val _riid: Rand[(Int, Int, Double)] = 
    for
      a <- State(RNG.int)
      b <- State(RNG.int)
      lowerBound  = Math.min(a, b)
      higherBound  = Math.max(a, b)
      x <- State (RNG.double)
    yield (lowerBound, higherBound, lowerBound + (higherBound-lowerBound).toDouble.abs * x)


  val __riid: Rand[(Int, Int, Double)] =
    State(RNG.int).flatMap { a =>
      State(RNG.int).flatMap { b =>
        val lowerBound = Math.min(a, b)
        val higherBound = Math.max(a, b)
        State(RNG.double).map { x =>
          val scaledX = lowerBound + (higherBound - lowerBound).toDouble.abs * x
          (lowerBound, higherBound, scaledX)
        }
      }
    }

  val riid: Rand[(Int, Int, Double)] = {
    val randomIntA: Rand[Int] = State(RNG.int)
    val randomIntB: Rand[Int] = State(RNG.int)
    val randomDouble: Rand[Double] = State(RNG.double)

    randomIntA.flatMap { a =>
      randomIntB.flatMap { b =>
        val lowerBound = Math.min(a, b)
        val higherBound = Math.max(a, b)

        randomDouble.map { x =>
          val scaledX = lowerBound + (higherBound - lowerBound).toDouble.abs * x
          (lowerBound, higherBound, scaledX)
        }
      }
    }
  }  
end Q4



object Q5:

  /*** TYPE EXTENSIONS
   *
   * Recall that we have two representations of Rand:
   *
   * - A type called RNG.Rand:
   *
   *   type Rand[+A] = RNG => (A, RNG)
   *
   * - And Rand as State:
   *
   *   type Rand[A] = State[RNG, A]
   *
   * In State.scala many more functions are available for RNG.Rand
   * than for State.Rand (because State is a more abstract interface
   * that can be used for other things, so it does not know that we
   * are dealing with RNG inside).  One example of such function is
   * `Rand.both`:
   *
   * def both[A,B] (ra: Rand[A], rb: Rand[B]): Rand[(A,B)]
   *
   * Q5. (5%)
   *
   * Make all functions of RNG.Rand available for State.Rand by a
   * suitable type conversion:
   */

  extension [A](r: RNG.Rand[A])
    def toStateRand: State[RNG, A] = State(r)
  

  // 2022 NOTE: 
  //
  // This question cannot be answered with the material we covered in
  // Scala 3. If you are interested, this is how it can be done in Scala 3 
  // https://docs.scala-lang.org/scala3/book/ca-implicit-conversions.html
  // but this material is not in the curriculum anymore


end Q5


/* Q6. (5%)
 *
 * Explain in English the mechanism you have used to achieve this. How
 * does your solution achieve the objective of Q5?
 *
 * Indicative length: 2-5 lines.
 */

// `toStateRand` converts a random number generator (`RNG.Rand[A]`) into a stateful computation (`State[RNG, A]`).
// - `RNG` represents the random number generator state.
// - `A` is the generated value.
// - `State` wraps the RNG logic, allowing composition with other state transformations.
// This bridges the gap between `RNG.Rand` and `State`
//


object Q7:

  /*** LAZY LISTS
   *
   * Let's assume that we have the following function 'size' implemented that
   * computes the lengths of a lazy list. (The function should be correct, no
   * point to seek traps in it.)
   */
  def size[A](s: LazyList[A]): Int =
    
    def f(s: LazyList[A], acc: Int): Int = 
      s match
        case LazyList.Cons(_, t) =>  f (t (), acc+1)
        case LazyList.Empty => acc
    f(s, 0)


  /* Q7. (5%)
   *
   * What is the problem with writing `size(s) >= 10` to check whether the lazy
   * list is at least 10 elements? Explain.
   */

  //If s is an infinite LazyList, the evaluation will never terminate.
  //Even if s is finite, evaluating all elements to determine its size is wasteful when we only need to check if there are at least 10 elements.

end Q7



object Q8:

  import Q7.size

  /**** A BETTER SIZE FOR LAZY STREAMS
   *
   * Q8. (5%)
   *
   * Implement a pure function `checkIfLongerEqThan` that checks whether
   * a stream is longer or equal than a given bound. Do not use the standard
   * library functions lengthCompare, sizeCompare, lenghtIs, or
   * sizeIs. You can use 'size' from above, or any other functions
   * from the course.
   */

  /**
   * checkIfLongerEqThan Explanation:
   * Checks if a LazyList is at least `n` elements long without fully evaluating it.
   */
  def checkIfLongerEqThan[A](s: LazyList[A])(n: Int): Boolean =
    s.drop(n - 1) match
      case LazyList.Empty => false
      case _ => true


  // Using size function above. But only taking n elements of the list, so it is not infinite
  def _checkIfLongerEqThan[A](s: LazyList[A])(n: Int): Boolean =
    size(s.take(n)) == n

  def __checkIfLongerEqThan_[A](s: LazyList[A])(n: Int): Boolean =
    (n == 0) || s.drop(n-1) != LazyList.Empty


end Q8



object Q9:

  import Q8.checkIfLongerEqThan
  import org.scalacheck.Prop.{forAll, forAllNoShrink}

  /*** TESTING
   *
   * Assume we have a solution for question Q8 (even if you skipped
   * it), so that we have a function:
   *
   *    checkIfLongerEqThan[A] (s: Stream[A]) (n: Int): Boolean
   *
   * that returns true if and only if the stream 's' has at least 'n'
   * elements.
   *
   *
   * Q9. (10%)
   *
   * Use it to write a property-based test that checks if the size of
   * every nonEmpty stream concatenated with itself is larger or equal
   * than 2.
   */

  class MySpec
    extends org.scalacheck.Properties("Q9"):


    //Generates a lazyList. The List have elements of 1 to 100
    given Arbitrary[LazyList[Int]] =
      Arbitrary { Gen.listOf(Gen.choose(1, 100))
        .map { l => LazyList(l*) } }

    // Guarantee non-empty LazyLists
    //given Arbitrary[LazyList[Int]] =
    //  Arbitrary { Gen.nonEmptyListOf(Gen.choose(1, 100))
    //    .map { l => LazyList(l*) } }    

    property("Q9: nonEmpty stream concatenation >= 2") =
      forAll { (s: LazyList[Int]) =>
        if s != LazyList.Empty then
          checkIfLongerEqThan(s.append(s))(2)
        else
          true
      }
    
    property("Q9_V2: nonEmpty stream concatenation >= 2") =
      forAll { (s: LazyList[Int]) =>
        s.headOption match
          case Some(_) => checkIfLongerEqThan(s.append(s))(2)
          case None    => true
      }

    //Only works if i use the other generator, that only gives nonempty LazyList instances
    //property("Q9: nonEmpty stream concatenation >= 2") =
    //  forAll { (s: LazyList[Int]) =>
    //    checkIfLongerEqThan(s.append(s))(2)
    //  }


end Q9



object Q10:

  import Par.*

  /*** PARALLEL OPTIONS
   *
   * Q10. (10%)
   *
   * Write a function which 'flattens' a Option[Par[A]] value to a
   * Par[Option[A]] value, for any type 'A'.
   */

  /**
   * flatten Function Explanation:
   * Converts an Option[Par[A]] into a Par[Option[A]].
   *
   * Example Input:
   * val opa: Option[Par[Int]] = Some(Par.unit(42))
   *
   * Step 1: Pattern match on the Option.
   * - Case Some(par): Run the Par computation and wrap it in Some.
   * - Case None: Return Par.unit(None).
   *
   * Final Output:
   * flatten(Some(Par.unit(42))) -> Par.unit(Some(42))
   * flatten(None) -> Par.unit(None)
   */
  def flatten[A](optionPar: Option[Par[A]]): Par[Option[A]] = 
    optionPar match
      case Some(par) => Par.map(par)(parallelOption => Some(parallelOption))
      case None => Par.unit(None)

end Q10



/*** Par[Option[_]] vs Option[Par[_]]
 *
 * Q11. (5%)
 *
 * Explain in English what the function from Q10 achieves.
 * Provide its user oriented description, not an explanation of the
 * implementation.
 * 
 *  * Par[Option[A]] represents a parallel computation that produces an Option[A] as its result.
 * In contrast, Option[Par[A]] represents an optional parallel computation.
 * The function from Q10 transforms an optional parallel computation into a parallel computation
 * that safely handles the optional value, ensuring it executes in a controlled parallel context.
 * This is useful for combining computations where some tasks might not exist or are conditionally defined.
 */





 
object Q12:

  /*** MONADS
   *
   * Recall the Identity Monad, the simplest possible monad, which
   * allows unit (identity) and mapping (function application) on any
   * type.
   */

  type Id[A] = A

  given idMonad: Monad[Id] = new:
    def unit[A] (a: => A): Id[A] = a
    extension [A](a: Id[A])
      override def flatMap[B](f: A => Id[B]): Id[B] = f(a)

  /* This function runs a loop 'in the monad M'.  It starts at the
   * initial value, then it applies the function 'body' as long as the
   * produced value of the type 'A' satisfies the predicate 'p'.
   *
   *
   * The code below computes a sum of list of integers.
   * (Warning: imperative code below)
   *
   * def sum(var l: List[Int]): Int =
   *   var result = 0
   *   while l.nonEmpty do
   *     result = result + l.head
   *     l = l.tail
   *   return result
   * 
   *
   * Q12. (10%)
   *
   * Convert the above imperative implementation of 'sum' into a pure one by
   * using the 'loop' function and the identity monad.  The implementation has
   * been started for you.  Complete it by replacing '???' (you may uncomment the
   * code):
   */

  import Q13.loop

  // An example solution
  type LI = (List[Int], Int)

  def sum(l: List[Int]): Int =
    val initial: LI = (l, 0)
    
    val body: LI => LI = {
      case (head :: tail, result) => (tail, result + head)
      case (Nil, result)          => (Nil, result) // Handles edge case explicitly
    }
    
    val p: LI => Boolean = {
      case (Nil, _) => false
      case _        => true
    }
    
    val result: LI = loop[LI, Id](initial)(body)(p)
    result._2


end Q12



object Q13:

  /*** LOOPING IN A MONAD
   *
   * Q13. (10%)
   *
   * Implement the function 'loop' from the above exercise that iterates a
   * calculation in a monad. Given an initial value of type A it checks (like a
   * while loop) whether it satisfies the predicate p. If not it returns the
   * value.
   */

  // Both solutions below are fine
  def loop_[A, M[_]: Monad](initial: M[A])(body: A => A)(p: A => Boolean): M[A] =
    for
      a      <- initial
      result <- if p(a) 
                then loop_ (summon[Monad[M]].unit(body(a)))(body)(p) 
                else initial
    yield result

  def _loop[A, M[_]: Monad](initial: M[A])(body: A => A) (p: A => Boolean): M[A] =
    summon[Monad[M]].flatMap(initial){ a =>
      if p(a) 
      then loop (summon[Monad[M]].unit(body(a)))(body)(p)
      else initial
    }

  def loop[A, M[_]: Monad](initial: M[A])(body: A => A)(p: A => Boolean): M[A] =
    summon[Monad[M]].flatMap(initial) { a =>
      if p(a)
      then loop(summon[Monad[M]].unit(body(a)))(body)(p)
      else summon[Monad[M]].unit(a) // Ensure termination by returning the last value
    }
    
end Q13

