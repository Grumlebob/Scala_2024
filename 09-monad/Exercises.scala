// Advanced Programming, A. Wąsowski, IT University of Copenhagen 
// Based on Functional Programming in Scala, 2nd Edition

package adpro.monad

import org.scalacheck.{Arbitrary, Prop}
import org.scalacheck.Prop.*
import org.scalacheck.Prop.given
import org.scalactic.Equality
import org.scalactic.TripleEquals.*

/***** Part I. Monoid */

trait Monoid[A]:

  def combine(a1: A, a2: A): A
  def empty: A
  
  // Some Laws for monoids (We place them here like in Parsers)

  object laws:

    /* The instance of Eqaulity type class will be used for equality
     * comparisons.  Normally the default equality is fine, but we need to
     * parameterize to handle endoMonoid (comparisons of functions).  See more
     * comments about that in the test of Ex03.01. */

    def associative(using Arbitrary[A], Equality[A]): Prop =
      forAll { (a1: A, a2: A, a3: A) =>
        combine(combine(a1, a2), a3) === combine(a1, combine(a2, a3)) 
      } :| "monoid-associative"

    /* The triple equality below (===) uses the eqA instance (by default the
     * same as == but we can override it, which is exploited in the test of
     * Exercise 3) */

    def unit(using Arbitrary[A], Equality[A]): Prop = 
      forAll { (a: A) =>
        (combine(a, empty) === a) :| "right-empty" && 
        (combine(empty, a) === a) :| "left-empty" 
      } :| "monoid-unit"

    def monoid (using Arbitrary[A], Equality[A]): Prop =
      (associative && unit) :| "monoid-laws"

end Monoid

val stringMonoid = new Monoid[String]:
  def combine(a1: String, a2: String) = a1 + a2
  val empty = ""

def listMonoid[A] = new Monoid[List[A]]:
  def combine(a1: List[A], a2: List[A]) = a1 ++ a2
  val empty = Nil


// Exercise 1

// Integer addition monoid
lazy val intAddition: Monoid[Int] = new Monoid[Int] {
  val empty = 0
  def combine(x: Int, y: Int): Int = x + y
}

// Integer multiplication monoid
lazy val intMultiplication: Monoid[Int] = new Monoid[Int] {
  val empty = 1
  def combine(x: Int, y: Int): Int = x * y
}

// Boolean OR monoid
lazy val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
  val empty = false
  def combine(x: Boolean, y: Boolean): Boolean = x || y
}

// Boolean AND monoid
lazy val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
  val empty = true
  def combine(x: Boolean, y: Boolean): Boolean = x && y
}


// Exercise 2

// Option Monoid where empty is None, and combine returns the first Some encountered
def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
  val empty = None
  def combine(x: Option[A], y: Option[A]): Option[A] = x orElse y
}

// Option Monoid Lift where A itself is a monoid
def optionMonoidLift[A: Monoid]: Monoid[Option[A]] = new Monoid[Option[A]] {
  val empty = None
  def combine(x: Option[A], y: Option[A]): Option[A] = (x, y) match {
    case (Some(a), Some(b)) => Some(summon[Monoid[A]].combine(a, b))
    case _ => x orElse y
  }
}


// Exercise 3
//function having the same argument and return type is called an endofunction.
//Write a monoid instance for endofunctions
// Endofunction Monoid: combining functions using composition
def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
  val empty: A => A = identity
  def combine(f: A => A, g: A => A): A => A = f andThen g
}


// Exercise 4 (tests exercises 1-2)

object MonoidEx4Spec extends org.scalacheck.Properties("exerc4"):

  property("Ex04.01: intAddition is a monoid") = intAddition.laws.monoid
  property("Ex04.02: intMultiplication is a monoid") = intMultiplication.laws.monoid
  property("Ex04.03: booleanOr is a monoid") = booleanOr.laws.monoid
  property("Ex04.04: booleanAnd is a monoid") = booleanAnd.laws.monoid
  property("Ex04.05: optionMonoid is a monoid") = optionMonoid[Int].laws.monoid
  property("Ex04.06: optionMonoidLift is a monoid") = optionMonoidLift[Int](using intAddition).laws.monoid

end MonoidEx4Spec




// Exercise 5

// We implement this as an extension, so that the exercises do not jump back
// and forth in the file. (This naturally belongs to the Monoid trait).

extension [B](mb: Monoid[B])
  def foldMap[A](as: List[A])(f: A => B): B = {
    val initAcc = mb.empty
    def folder(acc: B, a: A): B = mb.combine(acc, f(a))
    
    as.foldLeft(initAcc)(folder)
  }




// Exercise 6

/* We implement this as an extension, so that the exercises do not jump back
 * and forth in the file. (This naturally belongs to the Monoid trait).
 *
 * The triple equality (===) uses the Equality instance to check equality
 * (by default the same as == but we can override it). 
 *
 * Write property-based tests that test whether a function is a homomorphism between two
  sets, and then combine them in the test of isomorphism. 
 */

extension [A: Arbitrary: Equality](ma: Monoid[A])

  def homomorphism[B](transform: A => B)(mb: Monoid[B]): Prop =
    forAll { (a1: A, a2: A) =>
      mb.combine(transform(a1), transform(a2)) === transform(ma.combine(a1, a2))
    }

  def isomorphism[B: Arbitrary](to: A => B, from: B => A)(mb: Monoid[B]): Prop =
    homomorphism(to)(mb) && mb.homomorphism(from)(ma)



// Exercise 7 (tests for Exercise 6, written by the student)
/*
Use the laws from the tests in the previous exercises to establish an isomorphism between String and
List[Char] (or more precisely their monoids). Both monoids are already implemented above in the
file. A string can be translated to a list of characters using the toList method. The List.mkString
method with default arguments (no arguments) does the opposite conversion.
*/

object MonoidEx7Spec extends org.scalacheck.Properties("exerc7"):

  // Conversion functions for String <-> List[Char]
  val to: String => List[Char] = _.toList
  val from: List[Char] => String = _.mkString("")

  // Test to check if `stringMonoid` is isomorphic to `listMonoid[Char]`
  property("Ex07.01: stringMonoid is isomorphic to listMonoid[Char]") =
    stringMonoid.isomorphism(to, from)(listMonoid[Char])

end MonoidEx7Spec



// "Exercise 8 (tests for Exercise 1, written the by student)
/*
Use the morphism laws from Exercise 6 to show that the two Boolean monoids from
Exercise 1 above are isomorphic via the negation function (!).
*/

object MonoidEx8Spec extends org.scalacheck.Properties("exerc8"):
  
  // Negation function for isomorphism between booleanOr and booleanAnd
  def negate(b: Boolean): Boolean = !b

  property("Ex08.01: booleanOr is isomorphic to booleanAnd") =
    booleanOr.isomorphism(negate, negate)(booleanAnd)

end MonoidEx8Spec



// Exercise 9
//Implement a productMonoid that builds a monoid out of two monoids.

def productMonoid[A, B](ma: Monoid[A])(mb: Monoid[B]): Monoid[(A, B)] = new Monoid[(A, B)] {
  val empty = (ma.empty, mb.empty)
  def combine(x: (A, B), y: (A, B)): (A, B) = (ma.combine(x._1, y._1), mb.combine(x._2, y._2))
}


// Exercise 10 (tests for Exercise 9, written by the student)
/*
Test productMonoid using our monoid laws and Scalacheck. You need to provide
some concrete types for testing the product. We do not have generators of arbitrary monoids, so we
cannot quantify over them in the test. Instead, we can compose some concrete types, for instance
Option[Int] monoid with List[String] monoid. Run the resulting product monoid through our
monoid laws. You should not need to write any new laws. Just reuse the existing ones.
*/

object MonoidEx10Spec extends org.scalacheck.Properties("exer10"):
  
  // Test productMonoid with specific instances
  property("Ex10.01: productMonoid(optionMonoid[Int])(listMonoid[String]) gives a monoid") =
    productMonoid(optionMonoid[Int])(listMonoid[String]).laws.monoid

end MonoidEx10Spec



/* This will be used in the Foldable below: We can get the dual of any monoid
 * just by flipping the `combine`. */

def dual[A](m: Monoid[A]): Monoid[A] = new:
  def combine(x: A, y: A): A = m.combine(y, x)
  val empty = m.empty

/***** Part II. Foldable */

/* The functions foldRight and foldLeft below are little gems for self-study :). 
 * They resemble the foldLeft via foldRight exercise from the begining of
 * the course. */

trait Foldable[F[_]]:

  extension [A](as: F[A])

    def foldMap[B](f: A => B)(using mb: Monoid[B]): B

    def foldRight[B](z: B)(f: (A, B) => B): B =
      as.foldMap[B => B](f.curried)(using endoMonoid[B])(z)

    def foldLeft[B](z: B)(f: (B, A) => B): B =
      as.foldMap[B => B](a => b => f(b, a))(using dual(endoMonoid[B]))(z)

    def combineAll(using m: Monoid[A]): A =
      as.foldLeft(m.empty)(m.combine)

end Foldable

// Exercise 11
//Implement Foldable[List].

given foldableList: Foldable[List] with
  extension [A](as: List[A])
    def foldMap[B](transform: A => B)(using mb: Monoid[B]): B = {
      val initAcc = mb.empty
      def folder(acc: B, a: A): B = mb.combine(acc, transform(a))

      as.foldLeft(initAcc)(folder)
    }


 
// Exercise 12

// Note since Foldable[F] is a given, its extensions for as are visible
// (https://docs.scala-lang.org/scala3/reference/contextual/extension-methods.html)
/*

Any Foldable structure can be turned into a List. Write this conversion in a
generic way for any F[_]: Foldable and any A*/

extension [F[_]: Foldable, A] (as: F[A])
  def toList: List[A] = as.toListF
  def toListF: List[A] = {
    val initAcc = List.empty[A]
    def folder(acc: List[A], a: A): List[A] = a :: acc
    
    as.foldMap(List(_))(using listMonoid[A]).reverse
  }



/***** Part III. Functor */

trait Functor[F[_]]:

  extension [A](fa: F[A])
    def map[B](f: A => B): F[B]

  extension [A, B](fab: F[(A, B)]) 
    def distribute: (F[A], F[B])= 
      (fab.map(_._1), fab.map(_._2))

  extension [A, B](e: Either[F[A], F[B]]) 
    def codistribute: F[Either[A, B]] = e match
      case Left(fa) => fa.map { Left(_) }
      case Right(fb) => fb.map { Right(_) }

  object functorLaws:

    /* The triple equality below (===) uses the Equality instance to check
     * equality (by default the same as == but we can override it). */

    def map[A](using Arbitrary[F[A]], Equality[F[A]]): Prop =
      forAll { (fa: F[A]) => fa.map[A](identity[A]) === fa }

  end functorLaws

end Functor


// Exercise 13
//Implement an instance Functor[Option] of Functor for Option.
lazy val optionFunctor: Functor[Option] = new Functor[Option] {
  extension [A](fa: Option[A])
    def map[B](transform: A => B): Option[B] = fa.map(transform)
}



// this instance is provided
val listFunctor: Functor[List] = new:
  extension [A] (as: List[A])
    def map[B](f: A => B): List[B] = as.map(f)

object FunctorEx14Spec 
  extends org.scalacheck.Properties("exer14"):

  property("Ex14.01: listFunctor satisfies the map law") =
    listFunctor.functorLaws.map[Int]

  // Exercise 14
  /*
  Find the object functorLaws in the Functor trait (type class) and analyze how
the map law is implemented there, in a way that it can be used for any functor instance. The law
holds for any type A and a type constructor F[_], if we can generate arbitrary values of F[A] and test
for equality of F[A] values. Recall that Scalacheck needs to know that there exists an instance of
Arbtirary for F[A] in order to be able to generate random instances. And we need a way to test
equality to execute the property itself (the built-in equality == may not be suitable for all types, for
instance functions).
Below we show how to use the law to test that the ListFunctor is a functor (over integer lists). Note
that indeed the using parameter is not provided. Scalacheck defines the necessary given instances.
for List[_] and Int and these are matched automatically to arb∗ arguments at the call site.
Use the law to test that OptionFunctor of Exercise 13 is a functor.
*/

  property("Ex14.02: optionFunctor satisfies map law") =
    optionFunctor.functorLaws.map[Int]

end FunctorEx14Spec


/***** Part IV. Monad */

trait Monad[F[_]] 
  extends Functor[F]: 

  def unit[A](a: => A): F[A]
  
  extension [A](fa: F[A])
    def flatMap[B] (f: A => F[B]): F[B]

    def map[B](f: A => B): F[B] =
      fa.flatMap[B] { a => unit(f(a)) }

    def map2[B, C](fb: F[B])(f: (A, B) => C): F[C] =
      fa.flatMap { a => fb.map { b => f(a,b) } }
    
  object monadLaws:

    /* The triple equality below (===) uses the Equality instance to check
     * equality (by default the same as == but we can override it). */
 
    def associative[A, B, C](using Arbitrary[F[A]], Arbitrary[A => F[B]], 
      Arbitrary[B => F[C]], Equality[F[C]]): Prop = 
      forAll { (x: F[A], f: A => F[B], g: B => F[C]) =>
        val left = x.flatMap[B](f).flatMap[C](g)
        val right = x.flatMap[C] { a => f(a).flatMap[C](g) }
        (left === right) :| s"left:$left right:$right"
      }
 
    def identityRight[A](using Arbitrary[F[A]], Arbitrary[A => F[A]], Equality[F[A]]) =
      forAll { (x: F[A], f: A => F[A]) => 
        val result = x.flatMap[A](unit[A])
        (result === x) :| s"got:$result expected:$x" }
 
    def identityLeft[A: Arbitrary](using Arbitrary[A => F[A]], Equality[F[A]]) =
      forAll { (y: A, f: A => F[A]) =>
        val left = unit[A](y).flatMap[A](f) 
        val right = f(y) 
        (left === right) :| s"left:$left right:$right"
      }
 
    def identity[A: Arbitrary](using Arbitrary[F[A]], Arbitrary[A => F[A]], 
      Equality[F[A]]): Prop =
      { "identity left: " |: identityLeft[A]  } &&
      { "identity right:" |: identityRight[A] }
 
    def monad[A: Arbitrary, B, C] (using Arbitrary[F[A]], Arbitrary[A => F[A]],
      Arbitrary[A => F[B]], Arbitrary[B => F[C]]): Prop = 
      { "associative:" |: this.associative[A,B,C] } &&
      { "identity:   " |: this.identity[A] }

  end monadLaws
 
end Monad


// Exercise 15
//Write monad instances for Option and List. Remap standard library functions
//to the monad interface (or write them from scratch).

lazy val optionMonad: Monad[Option] = new Monad[Option] {
  def unit[A](a: => A): Option[A] = Option(a)
  extension [A](fa: Option[A])
    def flatMap[B](transform: A => Option[B]): Option[B] = fa.flatMap(transform)
}

lazy val listMonad: Monad[List] = new Monad[List] {
  def unit[A](a: => A): List[A] = List(a)
  extension [A](fa: List[A])
    def flatMap[B](transform: A => List[B]): List[B] = fa.flatMap(transform)
}



// Exercise 16 (tests for Exercise 15, written by the student)
/*
The object monadLaws in Exercises.scala shows the monad laws implemented
generically. The design is very similar to the one for functors. Compare this with the description of
laws in the book. Use these laws to add property tests for optionMonad and listMonad.*/
object FunctorEx16Spec 
  extends org.scalacheck.Properties("exer16"):

  given Arbitrary[Int] = Arbitrary(org.scalacheck.Gen.chooseNum(Int.MinValue, Int.MaxValue))

  // Testing monad laws for both optionMonad and listMonad
  property("Ex16.01: optionMonad is a monad") = optionMonad.monadLaws.monad
  property("Ex16.02: listMonad is a monad") = listMonad.monadLaws.monad

end FunctorEx16Spec


// Exercise 17
//
// We do this as an extension to maintain the linear sequence of exercises in
// the file (we would normally place it in the Monad trait).
/*
Implement sequence as an extension method for lists of monadic values. Express
it in terms of unit and map2. Sequence takes a list of monads and merges them into one, which
generates a list. Think about a monad as if it was a generator of values. The created monad will be a
generator of lists of values—each entry in the list generated by one of the input monads. The classic
type is:
def sequence[A] (lfa: List[F[A]]): F[List[A]]
but the exercise uses an extension, so that we do not have to jump around the file when adding
solutions.
Now this single implementation of sequence does all what all our previous implementations did—this
is truly mind-boggling! Use sequence to run some examples in the REPL. Sequence a list of instances
of the list monad, and a list of instances of the option monad. We could also use it to sequence
Gens, Pars, and Parsers, if we provided Monad instances for them. This exercise provides a key
intuition about the monad structure: A monad is a computational pattern for sequencing that is found
in amazingly many contexts.

*/

extension [F[_]](m: Monad[F])
  def sequence[A](fas: List[F[A]]): F[List[A]] = {
    val initAcc = m.unit(List.empty[A])
    
    def folder(fa: F[A], acc: F[List[A]]): F[List[A]] = 
      m.flatMap(fa)(a => m.map(acc)(a :: _)) // Folder function to combine each element

    fas.foldRight(initAcc)(folder)
  }



// Exercise 18
/*
Implement replicateM, which replicates a monad instance n times into an instance
of a list monad. This should be a method of the Monad trait.10
def replicateM[A](n: Int, ma: F[A]): F[List[A]]*/

extension [F[_]](m: Monad[F])
  def replicateM[A](n: Int, ma: F[A]): F[List[A]] =
    if (n <= 0) m.unit(List.empty[A])
    else m.flatMap(ma)(a => m.map(replicateM(n - 1, ma))(a :: _))


// Think how replicateM behaves for various choices of F. For example, how does it behave in the
// List monad? What about Option? Describe in your own words the general meaning of replicateM.
//
/*
replicateM(2, List(1, 2)) // Results in List(List(1, 1), List(1, 2), List(2, 1), List(2, 2))
replicateM(3, Some(5)) // Results in Some(List(5, 5, 5))
replicateM(3, None)    // Results in None

In general, replicateM takes a monadic value ma of type F[A] and repeats it n times, collecting the results in a List within the monadic context F. The final result is F[List[A]], which represents a monadic "list" of repeated values from ma.

The behavior of replicateM depends on the nature of the monad F:

For monads representing collections or multiple values (like List), replicateM explores all possible combinations of values.
For monads representing optionality or potential failure (like Option), replicateM short-circuits and returns a failure (None) if any intermediate computation fails.

*/


// Exercise 19

extension [F[_]](m: Monad[F])
  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => m.flatMap(f(a))(g)

