// Advanced Programming, A. Wąsowski, IT University of Copenhagen
// Based on Functional Programming in Scala, 2nd Edition

package adpro.prop

import adpro.state.*

val TODO = 42

// Exercise 1
//Create a new Simple random number generator of type RNG and seed it with 42.
lazy val rng1: RNG =
  RNG.Simple(TODO)

// Exercise 2
lazy val (x, rng2): (Int, RNG) = rng1.nextInt
lazy val y = rng2.nextInt._1

// Exercise 3

object Exercise_3 {

  //I have renamed forAll to avoid naming conflicts with our own forAll implementation
  import org.scalacheck.Prop.{forAll as scalaCheckForAll, forAllNoShrink}

  //a generator that produces non-empty lists of random integers between 0 and 100.
  val intList = org.scalacheck.Gen.listOf(org.scalacheck.Gen.choose(0, 100)).suchThat(_.nonEmpty)

  // Property 1: Minimum should be in the list
  def p1Min(minimum: List[Int] => Int): org.scalacheck.Prop =
/*
The second argument to forAll is a lambda (anonymous function)
The list is generated from the intList generator.
that represents the property to be tested. 
The lambda takes a generated list as its input and applies the test
The list is generated from the intList generator.*/

    scalaCheckForAll(intList) { (list: List[Int]) =>
      //Every list should contain the minimum element
      list.contains(minimum(list))
    }

  // Property 2: All elements should be greater than or equal to the minimum
  def p2Min(minimum: List[Int] => Int): org.scalacheck.Prop =
    scalaCheckForAll(intList) { (list: List[Int]) =>
      list.forall(_ >= minimum(list))
    }
}


end Exercise_3

// Exercise 4
/*
Exercise 4[M]. Implement && (conjunction) as an infix method of Prop, for the implementation of
Prop provided in the file. This method should create a new Prop object which checks to be true, if
both combined methods are true. Do not call check during an application of your &&, but only when
someone calls check on the conjoined property
*/

object Exercise_4:

  // This implementation of Prop is only used in Exercise 4
  trait Prop:
    def check: Boolean

    infix def &&(that: Prop): Prop = new Prop {
      //We must check both properties to see if they are true
      def check: Boolean = Prop.this.check && that.check
    }

end Exercise_4

opaque type Gen[+A] = State[RNG, A]

object Gen:
  
  extension [A](g: Gen[A]) 
    // Let's convert generator to streams of generators
    def toLazyList (rng: RNG): LazyList[A] =
      LazyList.unfold[A,RNG](rng)(rng => Some(g.run(rng)))

  /* Exercise 5
  Implement a test case generator Gen.choose. 
  It should generate integers in the range start to stopExclusive.
*/
  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    State { rng =>
      val (randomInt, nextRNG) = RNG.nonNegativeInt(rng)
      //We ensure by using modulo that the random number is within the range
      val nextElement = start + randomInt % (stopExclusive - start)
      (nextElement, nextRNG)
    }

  // Exercise 6

  def unit[A](a: => A): Gen[A] =
    //Unit just returns the value a, without changing the state of the RNG
    State { rng => (a, rng) }

  def boolean: Gen[Boolean] =
    State { rng =>
      val (i, nextRng) = RNG.nonNegativeInt(rng)
      val nextElement = i % 2 == 0
      (nextElement, nextRng)
    }

  def double: Gen[Double] =
    State { rng =>
      val (d, nextRNG) = RNG.double(rng) // Generates a value between 0.0 and 1.0
      val transformedDouble = (d * 4.0) - 2.0 // Transforms the range to [-2.0, 2.0]
      (transformedDouble, nextRNG)
    }

  // Exercise 7
  //Implement an extension method listOfN for Gen[A] that given an integer number n
  //returns a list of length n containing A elements, generated by the generator the method is called on
  
  extension [A](self: Gen[A])

    def listOfN(n: Int): Gen[List[A]] =
      State { rng =>
        def loop(count: Int, rng: RNG, acc: List[A]): (List[A], RNG) = {
          if (count <= 0) (acc, rng)
          else {
            val (nextElement, nextRNG) = self.run(rng)
            val nextCount = count -1
            val nextAcc = nextElement :: acc
            loop(nextCount, nextRNG, nextAcc)
          }
        }
        loop(n, rng, Nil)
      }
  /*
  Exercise 8 [H]. Explain in English why listOfN was implemented as an extension method of
                  Gen[A], not as a usual method.

ANSWER:
  By defining listOfN as an extension method, 
  we keep the core definition of Gen[A] simple and focused on generating individual values of type A.
  The listOfN functionality is more like an add-on behavior that works with any Gen[A], 
  rather than something that needs to be built into every Gen[A].
  In short: It provides flexibility to add this functionality only when needed, 
            without forcing every generator to have this method built-in by default.
*/
 
  
  /*
  Exercise 9 [H]. Implement flatMap for generators. Recall that flatMap allows to run another
generator on the result of the present one (this). Note that in the type below the parameter A is
implicitly bound, as this is also an extension method of Gen[A]:
  */

  extension [A](self: Gen[A])

    def flatMap[B](f: A => Gen[B]): Gen[B] =
      State { rng =>
        val (a, rng2) = self.run(rng)
        f(a).run(rng2)
      }

    // It will be convenient to also have map (uses flatMap)
    def map[B](f: A => B): Gen[B] = 
      self.flatMap { a => unit(f(a)) }


  // Exercise 10. Use flatMap to implement a more dynamic version of listOfN:

  extension [A](self: Gen[A])
    def listOf(size: Gen[Int]): Gen[List[A]] =
      //Size generates an int.
      //We flatMap the size to get the number of elements in the list
      //n is the number of elements in the list
      //Once we have the size n, we call self.listOfN(n), which generates a list of n elements using the self generator.
      size.flatMap(n => self.listOfN(n))


  // Exercise 11
  //Implement union, for combining two generators of the same type into one, by
  //pulling values from each generator with equal chance.

  extension [A](self: Gen[A])
    def union(that: Gen[A]): Gen[A] =
      Gen.boolean.flatMap(b => if (b) self else that)

end Gen

import Gen.*

object Exercise_12:

  // Exercise 12 (type classes, givens, using, summon)
  /*
  Reimplement functions listOfN[A] and ListOf[A] as top-level functions, not methods of Gen

  Type Classes:
    Type classes are a way to add new behavior to existing types without modifying them. 
    It is a way to achieve ad-hoc polymorphism in Scala. 
    Type classes are defined by a trait that represents the behavior and instances of the type class are created for specific types.
  
  Given: We can chose "what we summon" / "provides when write using"
    Given is used to define an implicit value or a type class instance. It is used to define a type class instance that can be used by the compiler to resolve implicit values.
  
  Using: The client doesn't have to provide this argument, as long as compiler can find it (from given keyword)
    Using is used to retrieve an implicit value that is available in the current scope. It is used to request an instance of a type class without passing it explicitly.

  Summon: "access the unnamed using argument" We use summon[Gen[A]] to fetch the generator for type A
    In Scala, summon is used to retrieve an implicit value (like a type class instance) that is available in the current scope. It's a way of requesting an instance of a type class without passing it explicitly.
  */

  def listOfN[A: Gen](n: Int): Gen[List[A]] =
    summon[Gen[A]].listOfN(n)
  
  def listOf[A: Gen](using genInt: Gen[Int]): Gen[List[A]] =
    summon[Gen[A]].listOf(genInt)

end Exercise_12


// (Exercise 13)
// Read the code below (and/or Section 8.1.6). You will find the
// exercise in the bottom of this fragment, marked with ??? as usual.

opaque type TestCases = Int //Example: If TestCases = 100, the property will be tested with 100 different randomly generated inputs.
opaque type FailedCase = String //description or details of a failure when a test case fails.
opaque type SuccessCount = Int //number of successful test cases.
opaque type MaxSize = Int //MaxSize of the list, such as 5 = [1,2,3,4,5]

/** The type of results returned by property testing. */
enum Result:
  case Passed
  case Falsified(failure: FailedCase, successes: SuccessCount)

  def isFalsified: Boolean = this match
    case Passed => false
    case Falsified(_, _) => true

end Result

import Result.{Passed, Falsified}

opaque type Prop = (MaxSize, TestCases, RNG) => Result

def randomLazyList[A](g: Gen[A])(rng: RNG): LazyList[A] =
  LazyList.unfold(rng)(rng => Some(g.run(rng)))

def buildMsg[A](s: A, e: Exception): String =
  s"test case: $s\n" +
  s"generated an exception: ${e.getMessage}\n" +
  s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

def forAll[A](as: Gen[A])(f: A => Boolean): Prop =
  (max, n, rng) => 
    randomLazyList(as)(rng)
      .zip(LazyList.from(0))
      .take(n)
      .map { (a, i) =>
             try if f(a) then Passed else Falsified(a.toString, i)
             catch case e: Exception => Falsified(buildMsg(a, e), i) }
      .find(_.isFalsified)
      .getOrElse(Passed)

def forAllNotSized[A] = forAll[A]
  
extension (self: Prop)

    /*
        Prop is a function that given some test cases and a random seed will produce a test result. Implement
Prop[A].&& and Prop[A].|| for composing Prop values. The former should succeed only
if both composed properties (this and that) succeed; the latter should fail only if both composed
properties fail.*/
  // Logical AND (&&)
  
  infix def &&(that: Prop): Prop = (maxSize: MaxSize, testCases: TestCases, rng: RNG) => {
    self(maxSize, testCases, rng) match {
      case Passed => that(maxSize, testCases, rng)
      case falsified: Falsified => falsified
    }
  }
  
  // Logical OR (||)
  infix def ||(that: Prop): Prop = (maxSize: MaxSize, testCases: TestCases, rng: RNG) => {
    self(maxSize, testCases, rng) match {
      case Passed => Passed
      case Falsified(_, _) => that(maxSize, testCases, rng)
    }
  }


// Exercise 14

/** The type of generators bounded by size 
 * 
 * Takes a int = maxSize 
 * returns a Gen[A]
 * 
 *
*/
opaque type SGen[+A] = Int => Gen[A]

extension [A](self: Gen[A])

  /*
  Implement a helper function for converting Gen to SGen.
You can add this as an extension method on Gen. This function should just ignore the size altogether,
so we have a ‘broken’ Gen but this way we can use our normal Gens as SGens*/
  def unsized: SGen[A] = _ => self

// Exercise 15

extension [A](self: Gen[A])

  /*
  Implement a sized list combinator. It should return an SGen instead of a Gen. The
  implementation should generate lists of the requested size.
*/ 
  def list: SGen[List[A]] = (size: Int) => self.listOfN(size)

// A sized implementation of prop, takes MaxSize to generate
// test cases of given size.  
//
// The code below also contains the `run` method for Prop - which
// provides a simple way to execute tests. Needed in the final
// exercise below.

object SGen:

  object Prop:

    def forAll[A](g: SGen[A])(f: A => Boolean): Prop =
      (max, n, rng) =>
        val casesPerSize = (n.toInt - 1)/max.toInt + 1
        val props: LazyList[Prop] =
          LazyList.from(0)
            .take((n.toInt min max.toInt) + 1)
            .map { i => forAllNotSized(g(i))(f) }
        val prop: Prop =
          props.map[Prop](p => (max, n, rng) => 
            p(max, casesPerSize, rng)).toList.reduce(_ && _)
        prop(max, n, rng)

  extension (self: Prop)
    def run(
      maxSize: MaxSize = 100,
      testCases: TestCases = 100,
      rng: RNG = RNG.Simple(System.currentTimeMillis)): Boolean =

      self(maxSize, testCases, rng) match
      case Result.Falsified(msg, n) =>
        println(s"\u001b[34m! Falsified after $n passed tests:\n $msg [message from our Prop framework]")
        false
      case Result.Passed =>
        println(s"\u001b[34m+ OK, passed $testCases tests. [message from our Prop framework]")
        true

end SGen

// Exercise 16

// Use this generator explicitly in the two properties
val nonEmptyList: SGen[List[Int]] =  
  (n: Int) => Gen.choose(-100, 100).listOfN(n max 1)

object Exercise_16 {

  /*
  Exercise 16[M]. Repeat Exercise 3 but now use our own testing framework to write the two tests.
In this exercise, the (ScalaCheck) test suite calls the tests you have written using our framework.
Messages from our framework are marked, and printed in blue, to reduce confusion. Notice that some
of these tests fail on purpose—when we test a testing framework we want to run both successful
and failing tests. Your grade is only decided based on the messages from ScalaCheck (so the usual
green/red/white messages).
  */

  import SGen.*

  // Property 1: The computed minimum should be an element of the list
  def p1Min(minimum: List[Int] => Int): Prop = SGen.Prop.forAll(nonEmptyList) { list =>
    list.contains(minimum(list))
  }

  // Property 2: All elements should be greater than or equal to the minimum
  def p2Min(minimum: List[Int] => Int): Prop = SGen.Prop.forAll(nonEmptyList) { list =>
    list.forall(_ >= minimum(list))
  }

}



end Exercise_16

// vim:cc=80:conceallevel=1
